module Json.Decode.Broken exposing
    ( parse
    , json, object, array, string, number, true, false, null, ws
    )

{-| Parse/decode broken JSON.


# Parsing

When successful, parsing returns a `Json.Encode.Value`. Use with with
`Json.Decode.decodeValue` to extract the information you need into your
application's data structures.

@docs parse


# Parser building blocks

A parser for a JSON _value_ is:

    Parser.oneOf [ object, array, string, number, true, false, null ]

According to the [specification][rfc7159], a JSON document is optional
whitespace, a JSON value (that `oneOf …` expression above), then more optional
whitespace – and that's what the `json` parser does. Hence parsing a compliant
JSON document is nothing more than:

    Parser.run json "…"

Use these building blocks to compose a parser for broken JSON as you need. If
you need to parse non-compliant quoted strings, for example, it might be best to
copy just the `string` code from this module into your project, and use the
other parsers in this module – `object`, `array`, and so on – to compose a new
parser.

[rfc7159]: https://tools.ietf.org/html/rfc7159

@docs json, object, array, string, number, true, false, null, ws

-}

import Json.Encode as Encode exposing (Value)
import Parser
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Parser
        , Step(..)
        , Trailing(..)
        , andThen
        , chompIf
        , chompWhile
        , getChompedString
        , lazy
        , loop
        , map
        , oneOf
        , problem
        , run
        , sequence
        , succeed
        , symbol
        , token
        )


{-| Parse the given JSON string.

This assumes a spec-compliant JSON string; it will choke on "broken" JSON. This
seems kind of weird for a package that's all about parsing broken JSON. However,
we all have to start somewhere. Read the code, copy it, modify it, make it work
for your use case.

Errors come straight from [elm/parser] and may not be super useful. Sorry.

[elm/parser]: https://package.elm-lang.org/packages/elm/parser/latest/

-}
parse : String -> Result (List DeadEnd) Value
parse =
    run json



-- Many of the names below mirror those from https://json.org/.


{-| Parser for JSON.

This is a JSON value surrounded by optional whitespace.

-}
json : Parser Value
json =
    succeed identity
        |. ws
        |= value
        |. ws


{-| Parser for a JSON value.
-}
value : Parser Value
value =
    oneOf
        [ object
        , array
        , string
        , number
        , true
        , false
        , null
        ]


yields : a -> Parser b -> Parser a
yields =
    always >> map


{-| Parser for a JSON 'true' literal.
-}
true : Parser Value
true =
    token "true" |> yields (Encode.bool True)


{-| Parser for a JSON 'false' literal.
-}
false : Parser Value
false =
    token "false" |> yields (Encode.bool False)


{-| Parser for a JSON 'null' literal.
-}
null : Parser Value
null =
    token "null" |> yields Encode.null


{-| Parser for a JSON object.
-}
object : Parser Value
object =
    succeed Encode.object
        |= sequence
            { start = "{"
            , separator = ","
            , end = "}"
            , spaces = ws
            , item = member
            , trailing = Forbidden
            }


member : Parser ( String, Value )
member =
    succeed Tuple.pair
        |= stringRaw
        |. ws
        |. symbol ":"
        |. ws
        |= lazy (\_ -> value)


{-| Parser for a JSON array.
-}
array : Parser Value
array =
    succeed (Encode.list identity)
        |= sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = ws
            , item = lazy (\_ -> value)
            , trailing = Forbidden
            }


{-| Parser for a quoted JSON string.

`string` and some of its helpers have been adapted from elm/parser's
`DoubleQuoteString` example.

-}
string : Parser Value
string =
    map Encode.string stringRaw


stringRaw : Parser String
stringRaw =
    succeed identity
        |. token "\""
        |= loop [] stringHelp


stringHelp : List String -> Parser (Step (List String) String)
stringHelp revChunks =
    let
        keepGoing chunk =
            Loop <| (chunk :: revChunks)

        endOfString _ =
            Done <| String.join "" <| List.reverse revChunks
    in
    oneOf
        [ succeed keepGoing
            |. token "\\"
            |= map String.fromChar escape
        , token "\""
            |> map endOfString
        , unescaped
            |> map keepGoing
        ]


escape : Parser Char
escape =
    oneOf
        [ token "\"" |> yields '"'
        , token "\\" |> yields '\\'
        , token "/" |> yields '/'
        , token "b" |> yields '\u{0008}'
        , token "f" |> yields '\u{000C}'
        , token "n" |> yields '\n'
        , token "r" |> yields '\u{000D}'
        , token "t" |> yields '\t'
        , succeed hexChar
            |. token "u"
            |= unicodeHexCode
        ]


unicodeHexCode : Parser String
unicodeHexCode =
    getChompedString <|
        succeed ()
            |. hexDigit
            |. hexDigit
            |. hexDigit
            |. hexDigit


hexChar : String -> Char
hexChar =
    -- ECMA 404 does not put a limit on the character ranges, i.e. it is
    -- possible to specify a character for which Unicode does not have a
    -- character assignment.
    String.foldl hexAcc 0 >> Char.fromCode


hexAcc : Char -> Int -> Int
hexAcc char total =
    let
        code =
            Char.toCode char
    in
    if 0x30 <= code && code <= 0x39 then
        16 * total + (code - 0x30)

    else if 0x41 <= code && code <= 0x46 then
        16 * total + (10 + code - 0x41)

    else
        16 * total + (10 + code - 0x61)


hexDigit : Parser ()
hexDigit =
    chompIf Char.isHexDigit


unescaped : Parser String
unescaped =
    let
        validChar code =
            (code >= 0x5D && code <= 0x0010FFFF)
                || (code >= 0x23 && code <= 0x5B)
                || (code >= 0x20 && code <= 0x21)
    in
    chompWhile (Char.toCode >> validChar)
        |> getChompedString


{-| Parser for a JSON number.
-}
number : Parser Value
number =
    let
        toFloat s =
            case String.toFloat s of
                Just f ->
                    succeed f

                Nothing ->
                    problem ("could not convert " ++ s ++ " to Float")
    in
    map Encode.float <|
        andThen toFloat <|
            getChompedString <|
                succeed ()
                    |. int
                    |. frac
                    |. exp


int : Parser String
int =
    let
        integer : Parser ()
        integer =
            oneOf
                [ succeed ()
                    |. oneNine
                    |. digitsMaybe
                , succeed ()
                    |. zero
                ]

        integerNegative : Parser ()
        integerNegative =
            succeed ()
                |. symbol "-"
                |. integer
    in
    getChompedString <|
        oneOf [ integer, integerNegative ]


digits : Parser ()
digits =
    succeed ()
        |. chompIf Char.isDigit
        |. chompWhile Char.isDigit


digitsMaybe : Parser ()
digitsMaybe =
    chompWhile Char.isDigit


digit : Parser ()
digit =
    chompIf Char.isDigit


zero : Parser ()
zero =
    chompIf ((==) '0')


oneNine : Parser ()
oneNine =
    chompIf (Char.toCode >> (\c -> c >= 0x31 && c <= 0x39))


frac : Parser ()
frac =
    oneOf
        [ succeed ()
            |. symbol "."
            |. digits
        , succeed ()
        ]


exp : Parser ()
exp =
    oneOf
        [ succeed ()
            |. oneOf [ symbol "e", symbol "E" ]
            |. sign
            |. digits
        , succeed ()
        ]


sign : Parser ()
sign =
    oneOf
        [ symbol "+"
        , symbol "-"
        , succeed ()
        ]


{-| Parser for JSON whitespace.

This is the whitespace that appears between significant elements of JSON, and
before and after JSON documents, not whitespace within quoted strings.

-}
ws : Parser ()
ws =
    chompWhile (\c -> c == '\t' || c == '\n' || c == '\u{000D}' || c == ' ')



-- Local Variables:
-- elm-format-elm-version: "0.19"
-- End:
