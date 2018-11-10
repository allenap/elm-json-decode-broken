module Json.Decode.BrokenSpec exposing
    ( parseArray
    , parseFalse
    , parseNothing
    , parseNull
    , parseNumber
    , parseObject
    , parseString
    , parseTrue
    , querying
    )

import Expect
import Json.Decode.Broken as Json
import Test exposing (..)


parseNothing : Test
parseNothing =
    test "parse the empty string" <|
        -- Ignore the details of the error message.
        \_ -> Json.parse "" |> Result.mapError (always Nothing) |> Expect.equal (Err Nothing)


parseNull : Test
parseNull =
    test "parse literal 'null'" <|
        \_ -> Json.parse "null" |> Expect.equal (Ok Json.Null)


parseFalse : Test
parseFalse =
    test "parse literal 'false'" <|
        \_ -> Json.parse "false" |> Expect.equal (Ok Json.False)


parseTrue : Test
parseTrue =
    test "parse literal 'true'" <|
        \_ -> Json.parse "true" |> Expect.equal (Ok Json.True)


parseNumber : Test
parseNumber =
    describe "parse numbers"
        [ test "integer" <|
            \_ ->
                Json.parse "123" |> Expect.equal (Ok <| Json.Number 123 Json.NoFrac Json.NoExp)
        , test "integer with exponent" <|
            \_ ->
                Json.parse "123e456"
                    |> Expect.equal
                        (Ok <| Json.Number 123 Json.NoFrac (Json.Exp Json.NoSign 456))
        , test "integer with plus-signed exponent" <|
            \_ ->
                Json.parse "123e+456"
                    |> Expect.equal
                        (Ok <| Json.Number 123 Json.NoFrac (Json.Exp Json.Plus 456))
        , test "integer with minus-signed exponent" <|
            \_ ->
                Json.parse "123e-456"
                    |> Expect.equal
                        (Ok <| Json.Number 123 Json.NoFrac (Json.Exp Json.Minus 456))
        , test "float" <|
            \_ ->
                Json.parse "123.456"
                    |> Expect.equal
                        (Ok <| Json.Number 123 (Json.Frac 456) Json.NoExp)
        , test "float with exponent" <|
            \_ ->
                Json.parse "123.456e789"
                    |> Expect.equal
                        (Ok <| Json.Number 123 (Json.Frac 456) (Json.Exp Json.NoSign 789))
        , test "float with plus-signed exponent" <|
            \_ ->
                Json.parse "123.456e+789"
                    |> Expect.equal
                        (Ok <| Json.Number 123 (Json.Frac 456) (Json.Exp Json.Plus 789))
        , test "float with minus-signed exponent" <|
            \_ ->
                Json.parse "123.456e-789"
                    |> Expect.equal
                        (Ok <| Json.Number 123 (Json.Frac 456) (Json.Exp Json.Minus 789))
        ]


parseString : Test
parseString =
    describe "parse strings"
        [ test "simple" <|
            \_ ->
                Json.parse "\"simple\"" |> Expect.equal (Ok <| Json.String "simple")
        , test "escape quote" <|
            \_ ->
                Json.parse "\"\\\"\"" |> Expect.equal (Ok <| Json.String "\"")
        , test "escape reverse solidus (backslash)" <|
            \_ ->
                Json.parse "\"\\\\\"" |> Expect.equal (Ok <| Json.String "\\")
        , test "escape solidus (forward-slash)" <|
            \_ ->
                Json.parse "\"\\/\"" |> Expect.equal (Ok <| Json.String "/")
        , test "escape backspace" <|
            \_ ->
                Json.parse "\"\\b\"" |> Expect.equal (Ok <| Json.String "\u{0008}")
        , test "escape formfeed" <|
            \_ ->
                Json.parse "\"\\f\"" |> Expect.equal (Ok <| Json.String "\u{000C}")
        , test "escape newline" <|
            \_ ->
                Json.parse "\"\\n\"" |> Expect.equal (Ok <| Json.String "\n")
        , test "escape carriage return" <|
            \_ ->
                Json.parse "\"\\r\"" |> Expect.equal (Ok <| Json.String "\u{000D}")
        , test "escape horizontal tab" <|
            \_ ->
                Json.parse "\"\\t\"" |> Expect.equal (Ok <| Json.String "\t")
        , test "escape unicode" <|
            \_ ->
                Json.parse "\"\\u0040\"" |> Expect.equal (Ok <| Json.String "@")
        , test "escape mixed" <|
            \_ ->
                Json.parse "\" \\\\ \\/ \\b \\f \\n \\r \\t \\u0040 \""
                    |> Expect.equal (Ok <| Json.String " \\ / \u{0008} \u{000C} \n \u{000D} \t @ ")
        ]


parseArray : Test
parseArray =
    describe "parse arrays"
        [ test "empty" <|
            \_ ->
                Json.parse "[]" |> Expect.equal (Ok <| Json.Array [])
        , test "with single element" <|
            \_ ->
                Json.parse "[null]" |> Expect.equal (Ok <| Json.Array [ Json.Null ])
        , test "with multiple elements" <|
            \_ ->
                Json.parse "[null, true, false, 1234, \"foo\"]"
                    |> Expect.equal
                        (Ok <|
                            Json.Array
                                [ Json.Null
                                , Json.True
                                , Json.False
                                , Json.Number 1234 Json.NoFrac Json.NoExp
                                , Json.String "foo"
                                ]
                        )
        , test "with nested values" <|
            \_ ->
                Json.parse "[ [null, true], [false, 1234], {\"foo\": \"bar\"} ]"
                    |> Expect.equal
                        (Ok <|
                            Json.Array
                                [ Json.Array
                                    [ Json.Null
                                    , Json.True
                                    ]
                                , Json.Array
                                    [ Json.False
                                    , Json.Number 1234 Json.NoFrac Json.NoExp
                                    ]
                                , Json.Object
                                    [ ( "foo", Json.String "bar" )
                                    ]
                                ]
                        )
        ]


parseObject : Test
parseObject =
    describe "parse objects"
        [ test "empty" <|
            \_ ->
                Json.parse "{}" |> Expect.equal (Ok <| Json.Object [])
        , test "with single member (object)" <|
            \_ ->
                Json.parse "{\"foo\": {}}"
                    |> Expect.equal
                        (Ok <|
                            Json.Object
                                [ ( "foo", Json.Object [] ) ]
                        )
        , test "with single member (array)" <|
            \_ ->
                Json.parse "{\"foo\": []}"
                    |> Expect.equal
                        (Ok <|
                            Json.Object
                                [ ( "foo", Json.Array [] ) ]
                        )
        , test "with single member (string)" <|
            \_ ->
                Json.parse "{\"foo\": \"bar\"}"
                    |> Expect.equal
                        (Ok <|
                            Json.Object
                                [ ( "foo", Json.String "bar" ) ]
                        )
        , test "with single member (number)" <|
            \_ ->
                Json.parse "{\"foo\": 123}"
                    |> Expect.equal
                        (Ok <|
                            Json.Object
                                [ ( "foo", Json.Number 123 Json.NoFrac Json.NoExp ) ]
                        )
        , test "with single member (true)" <|
            \_ ->
                Json.parse "{\"foo\": true}"
                    |> Expect.equal (Ok <| Json.Object [ ( "foo", Json.True ) ])
        , test "with single member (false)" <|
            \_ ->
                Json.parse "{\"foo\": false}"
                    |> Expect.equal (Ok <| Json.Object [ ( "foo", Json.False ) ])
        , test "with single member (null)" <|
            \_ ->
                Json.parse "{\"foo\": null}"
                    |> Expect.equal (Ok <| Json.Object [ ( "foo", Json.Null ) ])
        ]


querying : Test
querying =
    let
        value =
            Json.Object
                [ ( "foo"
                  , Json.Array
                        [ Json.String "bar"
                        , Json.Null
                        ]
                  )
                , ( "bar", Json.True )
                ]
    in
    describe "getting values"
        [ test "empty locator" <|
            \_ ->
                Json.get [] value
                    |> Expect.equal (Just value)
        , test "single locator" <|
            \_ ->
                Json.get [ Json.Key "bar" ] value
                    |> Expect.equal (Just Json.True)
        , test "single locator with no match" <|
            \_ ->
                Json.get [ Json.Key "wibble" ] value
                    |> Expect.equal Nothing
        , test "multiple locators" <|
            \_ ->
                Json.get [ Json.Key "foo", Json.Index 1 ] value
                    |> Expect.equal (Just Json.Null)
        , test "multiple locators with no match" <|
            \_ ->
                Json.get [ Json.Key "foo", Json.Index 2 ] value
                    |> Expect.equal Nothing
        , test "locator with mismatched type (index into object)" <|
            \_ ->
                Json.get [ Json.Index 0 ] value
                    |> Expect.equal Nothing
        , test "locators with mismatched type (key into array)" <|
            \_ ->
                Json.get [ Json.Key "foo", Json.Key "0" ] value
                    |> Expect.equal Nothing
        ]



-- Local Variables:
-- elm-format-elm-version: "0.19"
-- End:
