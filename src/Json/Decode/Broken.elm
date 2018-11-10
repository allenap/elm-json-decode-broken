module Json.Decode.Broken exposing
    ( parse, Value(..)
    , json, object, array, string, number, true, false, null, ws
    , Frac(..), Exp(..), Sign(..)
    )

{-| Decode broken JSON.


# Parsing

@docs parse, Value


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

@docs json, object, array, string, number, true, false, null, ws


# Numbers

@docs Frac, Exp, Sign

[rfc7159]: https://tools.ietf.org/html/rfc7159

-}

import Parser
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Parser
        , Step(..)
        , Trailing(..)
        , chompIf
        , chompWhile
        , getChompedString
        , lazy
        , loop
        , map
        , oneOf
        , run
        , sequence
        , succeed
        , symbol
        , token
        )


{-| Custom type for JSON values.

Mostly this is self-explanatory, but numbers might be interesting because no
effort is made to convert them to `Float` values. This means you can deal with
overflows or precision mismatches in broken JSON.

-}
type Value
    = Object (List ( String, Value ))
    | Array (List Value)
    | String String
    | Number Int Frac Exp
    | True
    | False
    | Null


{-| Parse the given JSON string.

Errors come straight from [elm/parser] and may not be super useful. It may be
worth changing this parser to use elm/parser's `Parser.Advanced` which allows
more control over errors.

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
    token "true" |> yields True


{-| Parser for a JSON 'false' literal.
-}
false : Parser Value
false =
    token "false" |> yields False


{-| Parser for a JSON 'null' literal.
-}
null : Parser Value
null =
    token "null" |> yields Null


{-| Parser for a JSON object.
-}
object : Parser Value
object =
    succeed Object
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
    succeed Array
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
    map String stringRaw


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
    succeed Number
        |= int
        |= frac
        |= exp


int : Parser Int
int =
    map digitsToInt <|
        getChompedString <|
            oneOf
                [ succeed ()
                    |. onenine
                    |. digits
                , succeed ()
                    |. digit
                , succeed ()
                    |. symbol "-"
                    |. onenine
                    |. digits
                , succeed ()
                    |. symbol "-"
                    |. digit
                ]


digits : Parser ()
digits =
    succeed ()
        |. chompIf Char.isDigit
        |. chompWhile Char.isDigit


digit : Parser ()
digit =
    chompIf Char.isDigit


onenine : Parser ()
onenine =
    chompIf (Char.toCode >> (\c -> c >= 0x31 && c <= 0x39))


{-| Does the number have a fractional component?

i.e. the optional part after the decimal point.

-}
type Frac
    = Frac Int
    | NoFrac


frac : Parser Frac
frac =
    oneOf
        [ succeed Frac
            |. symbol "."
            |= map digitsToInt (getChompedString digits)
        , succeed NoFrac
        ]


{-| Does the number have an exponent?

i.e. an optional suffix starting with `e` or `E` in the number.

-}
type Exp
    = Exp Sign Int
    | NoExp


exp : Parser Exp
exp =
    oneOf
        [ succeed Exp
            |. oneOf [ symbol "e", symbol "E" ]
            |= sign
            |= map digitsToInt (getChompedString digits)
        , succeed NoExp
        ]


{-| The sign of the exponent.
-}
type Sign
    = Plus
    | Minus
    | NoSign


sign : Parser Sign
sign =
    oneOf
        [ symbol "+" |> map (\_ -> Plus)
        , symbol "-" |> map (\_ -> Minus)
        , succeed NoSign
        ]


digitsToInt : String -> Int
digitsToInt =
    String.toInt >> Maybe.withDefault 0


toFloat : Int -> Frac -> Exp -> Maybe Float
toFloat i f e =
    let
        is =
            String.fromInt i

        fs =
            String.fromInt <|
                case f of
                    Frac f_ ->
                        f_

                    NoFrac ->
                        0

        es =
            String.fromInt <|
                case e of
                    Exp s_ e_ ->
                        case s_ of
                            Minus ->
                                negate e_

                            _ ->
                                e_

                    NoExp ->
                        0
    in
    String.toFloat
        (is ++ "." ++ fs ++ "e" ++ es)


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
