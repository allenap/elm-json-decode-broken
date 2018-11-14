module Json.Decode.Broken exposing
    ( parse
    , json
    , object, objectMember
    , array
    , string, stringRaw, escape, unicodeHexCode, unescaped
    , number, int, frac, exp, digit, digits, digitsMaybe, zero, oneNine
    , true, false, null, ws
    , hexChar, sign, yields
    )

{-| Parse/decode broken JSON.

When reading these docs or the code in this module, it might be useful to refer
to [json.org] (for the diagrams) or [RFC 8259] (for the official word). The
diagrams especially.

[json.org]: https://json.org/
[RFC 8259]: https://tools.ietf.org/html/rfc8259


# Parsing

When successful, parsing returns a `Json.Encode.Value`. Use with with
`Json.Decode.decodeValue` to extract the information you need into your
application's data structures.

@docs parse


# Top-level parsers

A parser for a JSON _value_ is:

    Parser.oneOf [ object, array, string, number, true, false, null ]

According to the [specification][rfc7159], a JSON document is: optional
whitespace, a JSON value (that `oneOf …` expression above), then more optional
whitespace. That's what the `json` parser does. Hence parsing a compliant JSON
document is:

    Parser.run json "…"

The component parsers are also exposed. Use them as building blocks to compose a
parser for broken JSON as you need. If you need to parse non-compliant quoted
strings, for example, it might be best to copy just the `string` code from this
module into your project, and use the other parsers in this module – `object`,
`array`, and so on – to compose a new parser.

For now the only exposed top-level parser is `json`.

[rfc7159]: https://tools.ietf.org/html/rfc7159

@docs json


# Parsers for objects

@docs object, objectMember


# Parsers for arrays

@docs array


# Parsers for strings

@docs string, stringRaw, escape, unicodeHexCode, unescaped


# Parsers for numbers

@docs number, int, frac, exp, digit, digits, digitsMaybe, zero, oneNine


# Parsers for the others

@docs true, false, null, ws


# Other functions

@ hexChar, yields

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


{-| Parser that, on succees, always returns `a`

For example:

    token "true" |> yields (Encode.bool True)

When the token "true" is matched, a boolean true value is yielded

-}
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
            , item = objectMember
            , trailing = Forbidden
            }


{-| Parser for a JSON object _member_.

Corresponds to the `member` production in the specifications.

-}
objectMember : Parser ( String, Value )
objectMember =
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


{-| Parser for a quoted JSON string.

The difference here is that this yields the actual `String` rather than a
re-encoded `Value`.

-}
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


{-| Parser for an escape sequence.

This does **not** include the leading escape prefix, i.e. `\\`.

-}
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


{-| Parser for a Unicode hexadecimal code.

E.g. "AbCd" or "1234" or "000D".

It will match exactly 4 hex digits, case-insensitive.

Goes well with [`hexChar`](#hexChar).

-}
unicodeHexCode : Parser String
unicodeHexCode =
    getChompedString <|
        succeed ()
            |. chompIf Char.isHexDigit
            |. chompIf Char.isHexDigit
            |. chompIf Char.isHexDigit
            |. chompIf Char.isHexDigit


{-| Convert a Unicode hexadecimal code to a `Char`.

Useful with [`unicodeHexCode`](#unicodeHexCode).

Note that ECMA 404 does not put a limit on the character ranges, i.e. it is
permissible in JSON to specify a character for which Unicode does not have a
character assignment. This leans on the behaviour of `Char.fromCode` to
determine what happens for codes not covered by Unicode.

-}
hexChar : String -> Char
hexChar =
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


{-| Parser for unescaped string contents.

The JSON specifications are specific about what characters are permissible in a
quoted string. Perhaps most interestingly, horizontal tabs, new-lines, and
carriage returns are **not** permitted; these **must** be escaped.

-}
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


{-| Parser for the integer portion of a JSON number.

    123.456e+78
    ^^^

-}
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


{-| Parser for one or more decimal digits.

This chomps characters; it does not yield them. Wrap with `getChompedString` to
obtain the matched string.

-}
digits : Parser ()
digits =
    succeed ()
        |. chompIf Char.isDigit
        |. chompWhile Char.isDigit


{-| Parser for _zero_ or more decimal digits.
-}
digitsMaybe : Parser ()
digitsMaybe =
    chompWhile Char.isDigit


{-| Parser for a single decimal digit.
-}
digit : Parser ()
digit =
    chompIf Char.isDigit


{-| Parser for a single decimal zero digit, `0`.
-}
zero : Parser ()
zero =
    chompIf ((==) '0')


{-| Parser for a single decimal digit between `1` and `9` inclusive.
-}
oneNine : Parser ()
oneNine =
    chompIf (Char.toCode >> (\c -> c >= 0x31 && c <= 0x39))


{-| Parser for an optional fractional portion of a JSON number.

    123.456e+78
       ^^^^

-}
frac : Parser ()
frac =
    oneOf
        [ succeed ()
            |. symbol "."
            |. digits
        , succeed ()
        ]


{-| Parser for an optional exponent portion of a JSON number.

    123.456e+78
           ^^^^

-}
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
