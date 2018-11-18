module Json.Decode.Broken exposing
    ( parse
    , parseWith, Config(..), defaultConfig
    , json
    , object, key
    , array
    , string, stringLiteral, escape, unicodeHexCode, unescaped
    , number, int, frac, exp, digit, digits, digitsMaybe, zero, oneNine
    , true, false, null, ws
    , hexChar, yields
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


# Custom parsing

This isn't going to get you total flexibility, but using [`Config`](#Config) and
co. will at least help you put together a consistent parser for JSON-like data.
By consistent, I mean that you override, say, the number parser and it will be
applied everywhere you might expect to see a number, be that at the top level,
or nested within any depths of objects or arrays.

@docs parseWith, Config, defaultConfig


# Top-level parsers

A parser for a JSON _value_ is:

    Parser.oneOf [ object, array, string, number, true, false, null ]

According to the [specification][rfc7159], a JSON document is: optional
whitespace, a JSON value (that `oneOf …` expression above), then more optional
whitespace. That's what the [`json`](#json) parser does. Hence parsing a
compliant JSON document is:

    Parser.run json "…"

The component parsers are also exposed. Use them as building blocks to compose a
parser for broken JSON as you need. If you need to parse non-compliant quoted
strings, for example, it might be best to copy just the [`string`](#string) code
from this module into your project, and use the other parsers in this module –
[`object`](#object), [`array`](#array), and so on – to compose a new parser.

For now the only exposed top-level parser is [`json`](#json).

[rfc7159]: https://tools.ietf.org/html/rfc7159

@docs json


# Parsers for objects

@docs object, key


# Parsers for arrays

@docs array


# Parsers for strings

@docs string, stringLiteral, escape, unicodeHexCode, unescaped


# Parsers for numbers

@docs number, int, frac, exp, digit, digits, digitsMaybe, zero, oneNine


# Parsers for the others

@docs true, false, null, ws


# Useful functions

@docs hexChar, yields

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


{-| Configuration for the parser.
-}
type Config
    = Config
        { json : Config -> Parser Value
        , value : Config -> Parser Value
        , object : Config -> Parser Value
        , array : Config -> Parser Value
        , key : Parser String
        , string : Parser Value
        , number : Parser Value
        , true : Parser Value
        , false : Parser Value
        , null : Parser Value
        , ws : Parser ()
        }


{-| Default configuration for the parser.
-}
defaultConfig : Config
defaultConfig =
    Config
        { json = json
        , value = value
        , object = object
        , array = array
        , key = key
        , string = string
        , number = number
        , true = true
        , false = false
        , null = null
        , ws = ws
        }


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
    parseWith defaultConfig


{-| Parse the given JSON string with a custom configuration.
-}
parseWith : Config -> String -> Result (List DeadEnd) Value
parseWith ((Config c) as config) =
    run (c.json config)


{-| Parser for JSON.

This is a JSON value surrounded by optional whitespace.

-}
json : Config -> Parser Value
json ((Config c) as config) =
    succeed identity
        |. c.ws
        |= c.value config
        |. c.ws


{-| Parser for a JSON value.
-}
value : Config -> Parser Value
value ((Config c) as config) =
    oneOf
        [ c.object config
        , c.array config
        , c.string
        , c.number
        , c.true
        , c.false
        , c.null
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
object : Config -> Parser Value
object ((Config c) as config) =
    succeed Encode.object
        |= sequence
            { start = "{"
            , separator = ","
            , end = "}"
            , spaces = c.ws
            , item = objectMember config
            , trailing = Forbidden
            }


{-| Parser for a JSON object _member_.

Corresponds to the `member` production in the specifications.

-}
objectMember : Config -> Parser ( String, Value )
objectMember ((Config c) as config) =
    succeed Tuple.pair
        |= c.key
        |. c.ws
        |. symbol ":"
        |. c.ws
        |= lazy (\_ -> c.value config)


{-| Parser for a JSON object _key_.
-}
key : Parser String
key =
    stringLiteral


{-| Parser for a JSON array.
-}
array : Config -> Parser Value
array ((Config c) as config) =
    succeed (Encode.list identity)
        |= sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = c.ws
            , item = lazy (\_ -> c.value config)
            , trailing = Forbidden
            }


{-| Parser for a quoted JSON string.

[`string`](#string) and some of its helpers have been adapted from elm/parser's
`DoubleQuoteString` example.

-}
string : Parser Value
string =
    map Encode.string stringLiteral


{-| Parser for a quoted JSON string literal.

The difference here is that this yields the actual `String` rather than a
re-encoded `Value`. This is also used for object keys which need to be captured
as `String`.

-}
stringLiteral : Parser String
stringLiteral =
    succeed identity
        |. token "\""
        |= loop [] stringLiteralHelp


stringLiteralHelp : List String -> Parser (Step (List String) String)
stringLiteralHelp revChunks =
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
