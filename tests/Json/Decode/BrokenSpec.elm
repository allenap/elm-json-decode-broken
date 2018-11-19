module Json.Decode.BrokenSpec exposing
    ( decodeToRecord
    , parseArray
    , parseCustomNumbers
    , parseFalse
    , parseNothing
    , parseNull
    , parseNumber
    , parseObject
    , parseRelaxedStrings
    , parseString
    , parseTrue
    )

import Expect
import Fuzz
import Json.Decode as Decode
import Json.Decode.Broken as Broken
import Json.Encode as Encode
import Parser exposing ((|.), Parser)
import Test exposing (..)


parseAndDecode : String -> Decode.Decoder a -> Result String a
parseAndDecode json decoder =
    case Broken.parse json of
        Ok value ->
            Decode.decodeValue decoder value
                |> Result.mapError Decode.errorToString

        Err error ->
            Err <| Debug.toString error


parseDecodeAndEncode : String -> Result String String
parseDecodeAndEncode json =
    parseAndDecode json Decode.value
        |> Result.map (Encode.encode 0)


parseNothing : Test
parseNothing =
    test "parse the empty string" <|
        -- Ignore the details of the error message.
        \_ -> Broken.parse "" |> Result.mapError (always Nothing) |> Expect.equal (Err Nothing)


parseNull : Test
parseNull =
    test "parse literal 'null'" <|
        \_ -> parseAndDecode "null" (Decode.null ()) |> Expect.equal (Ok ())


parseFalse : Test
parseFalse =
    test "parse literal 'false'" <|
        \_ -> parseAndDecode "false" Decode.bool |> Expect.equal (Ok False)


parseTrue : Test
parseTrue =
    test "parse literal 'true'" <|
        \_ -> parseAndDecode "true" Decode.bool |> Expect.equal (Ok True)


parseNumber : Test
parseNumber =
    describe "parse numbers"
        [ fuzz Fuzz.float "integers" <|
            \f ->
                parseAndDecode (String.fromFloat f) Decode.float |> Expect.equal (Ok f)
        , test "integer with exponent" <|
            \_ ->
                parseAndDecode "123e45" Decode.float |> Expect.equal (Ok 1.23e47)
        , test "integer with plus-signed exponent" <|
            \_ ->
                parseAndDecode "123e+45" Decode.float |> Expect.equal (Ok 1.23e47)
        , test "integer with minus-signed exponent" <|
            \_ ->
                parseAndDecode "123e-45" Decode.float |> Expect.equal (Ok 1.23e-43)
        , test "float" <|
            \_ ->
                parseAndDecode "123.456" Decode.float |> Expect.equal (Ok 123.456)
        , test "float with exponent" <|
            \_ ->
                parseAndDecode "123.456e78" Decode.float |> Expect.equal (Ok 1.23456e80)
        , test "float with plus-signed exponent" <|
            \_ ->
                parseAndDecode "123.456e+78" Decode.float |> Expect.equal (Ok 1.23456e80)
        , test "float with minus-signed exponent" <|
            \_ ->
                parseAndDecode "123.456e-78" Decode.float |> Expect.equal (Ok 1.23456e-76)
        ]


parseString : Test
parseString =
    describe "parse strings"
        [ test "simple" <|
            \_ ->
                parseAndDecode "\"simple\"" Decode.string |> Expect.equal (Ok "simple")
        , test "escape quote" <|
            \_ ->
                parseAndDecode "\"\\\"\"" Decode.string |> Expect.equal (Ok "\"")
        , test "escape reverse solidus (backslash)" <|
            \_ ->
                parseAndDecode "\"\\\\\"" Decode.string |> Expect.equal (Ok "\\")
        , test "escape solidus (forward-slash)" <|
            \_ ->
                parseAndDecode "\"\\/\"" Decode.string |> Expect.equal (Ok "/")
        , test "escape backspace" <|
            \_ ->
                parseAndDecode "\"\\b\"" Decode.string |> Expect.equal (Ok "\u{0008}")
        , test "escape formfeed" <|
            \_ ->
                parseAndDecode "\"\\f\"" Decode.string |> Expect.equal (Ok "\u{000C}")
        , test "escape newline" <|
            \_ ->
                parseAndDecode "\"\\n\"" Decode.string |> Expect.equal (Ok "\n")
        , test "escape carriage return" <|
            \_ ->
                parseAndDecode "\"\\r\"" Decode.string |> Expect.equal (Ok "\u{000D}")
        , test "escape horizontal tab" <|
            \_ ->
                parseAndDecode "\"\\t\"" Decode.string |> Expect.equal (Ok "\t")
        , test "escape unicode" <|
            \_ ->
                parseAndDecode "\"\\u0040\"" Decode.string |> Expect.equal (Ok "@")
        , test "escape mixed" <|
            \_ ->
                parseAndDecode "\" \\\\ \\/ \\b \\f \\n \\r \\t \\u0040 \"" Decode.string
                    |> Expect.equal (Ok " \\ / \u{0008} \u{000C} \n \u{000D} \t @ ")
        ]


parseArray : Test
parseArray =
    describe "parse arrays"
        [ test "empty" <|
            \_ ->
                parseAndDecode "[]" (Decode.list Decode.value) |> Expect.equal (Ok [])
        , test "with single element" <|
            \_ ->
                parseAndDecode "[null]" (Decode.list <| Decode.null ()) |> Expect.equal (Ok [ () ])
        , test "with multiple elements" <|
            \_ ->
                parseAndDecode "[null, true, false, 1234, \"foo\"]" (Decode.list Decode.value)
                    |> Result.map (List.map (Encode.encode 2))
                    |> Expect.equal
                        (Ok
                            [ "null"
                            , "true"
                            , "false"
                            , "1234"
                            , "\"foo\""
                            ]
                        )
        , test "with nested values" <|
            \_ ->
                parseDecodeAndEncode "[ [null, true], [false, 1234], {\"foo\": \"bar\"} ]"
                    |> Expect.equal (Ok "[[null,true],[false,1234],{\"foo\":\"bar\"}]")
        ]


parseObject : Test
parseObject =
    describe "parse objects"
        [ test "empty" <|
            \_ ->
                parseDecodeAndEncode "{   }"
                    |> Expect.equal (Ok "{}")
        , test "with single member (object)" <|
            \_ ->
                parseDecodeAndEncode "{ \"foo\" : {} }"
                    |> Expect.equal (Ok "{\"foo\":{}}")
        , test "with single member (array)" <|
            \_ ->
                parseDecodeAndEncode "{ \"foo\" : [] }"
                    |> Expect.equal (Ok "{\"foo\":[]}")
        , test "with single member (string)" <|
            \_ ->
                parseDecodeAndEncode "{\"foo\" :\"bar\"}"
                    |> Expect.equal (Ok "{\"foo\":\"bar\"}")
        , test "with single member (number)" <|
            \_ ->
                parseDecodeAndEncode "{\"foo\" \u{000D}: \t 123\n}"
                    |> Expect.equal (Ok "{\"foo\":123}")
        , test "with single member (true)" <|
            \_ ->
                parseDecodeAndEncode "{\"foo\": true}"
                    |> Expect.equal (Ok "{\"foo\":true}")
        , test "with single member (false)" <|
            \_ ->
                parseDecodeAndEncode "{\"foo\": false}"
                    |> Expect.equal (Ok "{\"foo\":false}")
        , test "with single member (null)" <|
            \_ ->
                parseDecodeAndEncode "{\"foo\": null}"
                    |> Expect.equal (Ok "{\"foo\":null}")
        ]


type alias Record =
    { aaa : List String
    , bbb : Float
    , ccc : Bool
    , ddd : Bool
    , eee : ()
    }


decodeToRecord : Test
decodeToRecord =
    let
        json =
            "{\"aaa\" : [ \"foo\", \"bar\" ] , \"bbb\" : 12.3e4 , "
                ++ "\"ccc\": true , \"ddd\"   :false , \"eee\" : null }"

        decoder =
            Decode.map5 Record
                (Decode.field "aaa" (Decode.list Decode.string))
                (Decode.field "bbb" Decode.float)
                (Decode.field "ccc" Decode.bool)
                (Decode.field "ddd" Decode.bool)
                (Decode.field "eee" (Decode.null ()))
    in
    test "decode to an Elm record" <|
        \_ ->
            parseAndDecode json decoder
                |> Expect.equal
                    (Ok
                        { aaa = [ "foo", "bar" ]
                        , bbb = 123000
                        , ccc = True
                        , ddd = False
                        , eee = ()
                        }
                    )



--
-- CUSTOM PARSERS
--


parseAndDecodeWith : Broken.Config -> String -> Decode.Decoder a -> Result String a
parseAndDecodeWith config json decoder =
    case Broken.parseWith config json of
        Ok value ->
            Decode.decodeValue decoder value
                |> Result.mapError Decode.errorToString

        Err error ->
            Err <| Debug.toString error


parseDecodeAndEncodeWith : Broken.Config -> String -> Result String String
parseDecodeAndEncodeWith config json =
    parseAndDecodeWith config json Decode.value
        |> Result.map (Encode.encode 0)


englishNumbers : Parser Encode.Value
englishNumbers =
    Parser.map Encode.float <|
        Parser.oneOf
            [ Parser.token "one" |> Broken.yields 1
            , Parser.token "two" |> Broken.yields 2
            , Parser.token "three" |> Broken.yields 3
            ]


englishNumbersConfig : Broken.Config
englishNumbersConfig =
    case Broken.defaultConfig of
        Broken.Config c ->
            Broken.Config { c | number = englishNumbers }


parseCustomNumbers : Test
parseCustomNumbers =
    describe "parse custom numbers"
        [ test "parse literal 'one'" <|
            \_ ->
                parseAndDecodeWith englishNumbersConfig "one" Decode.float
                    |> Expect.equal (Ok 1)
        , test "parse literal 'two' in array" <|
            \_ ->
                parseAndDecodeWith englishNumbersConfig "[two]" (Decode.list Decode.float)
                    |> Expect.equal (Ok [ 2 ])
        , test "parse literal 'three' in object" <|
            \_ ->
                parseAndDecodeWith englishNumbersConfig "{\"a\": three}" (Decode.keyValuePairs Decode.float)
                    |> Expect.equal (Ok [ ( "a", 3 ) ])
        ]


relaxedStringUnescaped : Parser String
relaxedStringUnescaped =
    Parser.oneOf
        [ Broken.unescaped
        , Parser.symbol "\u{000D}" |> Broken.yields "\u{000D}"
        , Parser.symbol "\n" |> Broken.yields "\n"
        ]


relaxedStringLiteral : Parser String
relaxedStringLiteral =
    Broken.stringLiteral relaxedStringUnescaped Broken.escape


relaxedString : Parser Encode.Value
relaxedString =
    Parser.map Encode.string relaxedStringLiteral


relaxedStringConfig : Broken.Config
relaxedStringConfig =
    case Broken.defaultConfig of
        Broken.Config c ->
            Broken.Config { c | string = relaxedString }


parseRelaxedStrings : Test
parseRelaxedStrings =
    describe "parse 'relaxed' strings"
        [ test "parse literal" <|
            \_ ->
                parseAndDecodeWith relaxedStringConfig "\"foo\u{000D}\nbar\u{000D}\n\"" Decode.string
                    |> Expect.equal (Ok "foo\u{000D}\nbar\u{000D}\n")
        , test "parse literal in array" <|
            \_ ->
                parseAndDecodeWith relaxedStringConfig "[\"foo\u{000D}\nbar\u{000D}\n\"]" (Decode.list Decode.string)
                    |> Expect.equal (Ok [ "foo\u{000D}\nbar\u{000D}\n" ])
        , test "parse literal in object" <|
            \_ ->
                parseAndDecodeWith relaxedStringConfig
                    "{\"a\": \"foo\u{000D}\nbar\u{000D}\n\"}"
                    (Decode.keyValuePairs Decode.string)
                    |> Expect.equal (Ok [ ( "a", "foo\u{000D}\nbar\u{000D}\n" ) ])
        ]



-- Local Variables:
-- elm-format-elm-version: "0.19"
-- End:
