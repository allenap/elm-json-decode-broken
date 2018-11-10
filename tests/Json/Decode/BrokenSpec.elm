module Json.Decode.BrokenSpec exposing
    ( encoding
    , parseArray
    , parseFalse
    , parseNothing
    , parseNull
    , parseNumber
    , parseObject
    , parseString
    , parseTrue
    )

import Expect
import Json.Decode
import Json.Decode.Broken as Broken
import Test exposing (..)


parseNothing : Test
parseNothing =
    test "parse the empty string" <|
        -- Ignore the details of the error message.
        \_ -> Broken.parse "" |> Result.mapError (always Nothing) |> Expect.equal (Err Nothing)


parseNull : Test
parseNull =
    test "parse literal 'null'" <|
        \_ -> Broken.parse "null" |> Expect.equal (Ok Broken.Null)


parseFalse : Test
parseFalse =
    test "parse literal 'false'" <|
        \_ -> Broken.parse "false" |> Expect.equal (Ok Broken.False)


parseTrue : Test
parseTrue =
    test "parse literal 'true'" <|
        \_ -> Broken.parse "true" |> Expect.equal (Ok Broken.True)


parseNumber : Test
parseNumber =
    describe "parse numbers"
        [ test "integer" <|
            \_ ->
                Broken.parse "123" |> Expect.equal (Ok <| Broken.Number 123 Broken.NoFrac Broken.NoExp)
        , test "integer with exponent" <|
            \_ ->
                Broken.parse "123e456"
                    |> Expect.equal
                        (Ok <| Broken.Number 123 Broken.NoFrac (Broken.Exp Broken.NoSign 456))
        , test "integer with plus-signed exponent" <|
            \_ ->
                Broken.parse "123e+456"
                    |> Expect.equal
                        (Ok <| Broken.Number 123 Broken.NoFrac (Broken.Exp Broken.Plus 456))
        , test "integer with minus-signed exponent" <|
            \_ ->
                Broken.parse "123e-456"
                    |> Expect.equal
                        (Ok <| Broken.Number 123 Broken.NoFrac (Broken.Exp Broken.Minus 456))
        , test "float" <|
            \_ ->
                Broken.parse "123.456"
                    |> Expect.equal
                        (Ok <| Broken.Number 123 (Broken.Frac 456) Broken.NoExp)
        , test "float with exponent" <|
            \_ ->
                Broken.parse "123.456e789"
                    |> Expect.equal
                        (Ok <| Broken.Number 123 (Broken.Frac 456) (Broken.Exp Broken.NoSign 789))
        , test "float with plus-signed exponent" <|
            \_ ->
                Broken.parse "123.456e+789"
                    |> Expect.equal
                        (Ok <| Broken.Number 123 (Broken.Frac 456) (Broken.Exp Broken.Plus 789))
        , test "float with minus-signed exponent" <|
            \_ ->
                Broken.parse "123.456e-789"
                    |> Expect.equal
                        (Ok <| Broken.Number 123 (Broken.Frac 456) (Broken.Exp Broken.Minus 789))
        ]


parseString : Test
parseString =
    describe "parse strings"
        [ test "simple" <|
            \_ ->
                Broken.parse "\"simple\"" |> Expect.equal (Ok <| Broken.String "simple")
        , test "escape quote" <|
            \_ ->
                Broken.parse "\"\\\"\"" |> Expect.equal (Ok <| Broken.String "\"")
        , test "escape reverse solidus (backslash)" <|
            \_ ->
                Broken.parse "\"\\\\\"" |> Expect.equal (Ok <| Broken.String "\\")
        , test "escape solidus (forward-slash)" <|
            \_ ->
                Broken.parse "\"\\/\"" |> Expect.equal (Ok <| Broken.String "/")
        , test "escape backspace" <|
            \_ ->
                Broken.parse "\"\\b\"" |> Expect.equal (Ok <| Broken.String "\u{0008}")
        , test "escape formfeed" <|
            \_ ->
                Broken.parse "\"\\f\"" |> Expect.equal (Ok <| Broken.String "\u{000C}")
        , test "escape newline" <|
            \_ ->
                Broken.parse "\"\\n\"" |> Expect.equal (Ok <| Broken.String "\n")
        , test "escape carriage return" <|
            \_ ->
                Broken.parse "\"\\r\"" |> Expect.equal (Ok <| Broken.String "\u{000D}")
        , test "escape horizontal tab" <|
            \_ ->
                Broken.parse "\"\\t\"" |> Expect.equal (Ok <| Broken.String "\t")
        , test "escape unicode" <|
            \_ ->
                Broken.parse "\"\\u0040\"" |> Expect.equal (Ok <| Broken.String "@")
        , test "escape mixed" <|
            \_ ->
                Broken.parse "\" \\\\ \\/ \\b \\f \\n \\r \\t \\u0040 \""
                    |> Expect.equal (Ok <| Broken.String " \\ / \u{0008} \u{000C} \n \u{000D} \t @ ")
        ]


parseArray : Test
parseArray =
    describe "parse arrays"
        [ test "empty" <|
            \_ ->
                Broken.parse "[]" |> Expect.equal (Ok <| Broken.Array [])
        , test "with single element" <|
            \_ ->
                Broken.parse "[null]" |> Expect.equal (Ok <| Broken.Array [ Broken.Null ])
        , test "with multiple elements" <|
            \_ ->
                Broken.parse "[null, true, false, 1234, \"foo\"]"
                    |> Expect.equal
                        (Ok <|
                            Broken.Array
                                [ Broken.Null
                                , Broken.True
                                , Broken.False
                                , Broken.Number 1234 Broken.NoFrac Broken.NoExp
                                , Broken.String "foo"
                                ]
                        )
        , test "with nested values" <|
            \_ ->
                Broken.parse "[ [null, true], [false, 1234], {\"foo\": \"bar\"} ]"
                    |> Expect.equal
                        (Ok <|
                            Broken.Array
                                [ Broken.Array
                                    [ Broken.Null
                                    , Broken.True
                                    ]
                                , Broken.Array
                                    [ Broken.False
                                    , Broken.Number 1234 Broken.NoFrac Broken.NoExp
                                    ]
                                , Broken.Object
                                    [ ( "foo", Broken.String "bar" )
                                    ]
                                ]
                        )
        ]


parseObject : Test
parseObject =
    describe "parse objects"
        [ test "empty" <|
            \_ ->
                Broken.parse "{}" |> Expect.equal (Ok <| Broken.Object [])
        , test "with single member (object)" <|
            \_ ->
                Broken.parse "{\"foo\": {}}"
                    |> Expect.equal
                        (Ok <|
                            Broken.Object
                                [ ( "foo", Broken.Object [] ) ]
                        )
        , test "with single member (array)" <|
            \_ ->
                Broken.parse "{\"foo\": []}"
                    |> Expect.equal
                        (Ok <|
                            Broken.Object
                                [ ( "foo", Broken.Array [] ) ]
                        )
        , test "with single member (string)" <|
            \_ ->
                Broken.parse "{\"foo\": \"bar\"}"
                    |> Expect.equal
                        (Ok <|
                            Broken.Object
                                [ ( "foo", Broken.String "bar" ) ]
                        )
        , test "with single member (number)" <|
            \_ ->
                Broken.parse "{\"foo\": 123}"
                    |> Expect.equal
                        (Ok <|
                            Broken.Object
                                [ ( "foo", Broken.Number 123 Broken.NoFrac Broken.NoExp ) ]
                        )
        , test "with single member (true)" <|
            \_ ->
                Broken.parse "{\"foo\": true}"
                    |> Expect.equal (Ok <| Broken.Object [ ( "foo", Broken.True ) ])
        , test "with single member (false)" <|
            \_ ->
                Broken.parse "{\"foo\": false}"
                    |> Expect.equal (Ok <| Broken.Object [ ( "foo", Broken.False ) ])
        , test "with single member (null)" <|
            \_ ->
                Broken.parse "{\"foo\": null}"
                    |> Expect.equal (Ok <| Broken.Object [ ( "foo", Broken.Null ) ])
        ]


type alias Something =
    { aaa : List String
    , bbb : Float
    , ccc : Bool
    , ddd : Bool
    , eee : ()
    }


encoding : Test
encoding =
    let
        value =
            Broken.Object
                [ ( "aaa", Broken.Array [ Broken.String "foo", Broken.String "bar" ] )
                , ( "bbb", Broken.Number 1 (Broken.Frac 2) (Broken.Exp Broken.Plus 3) )
                , ( "ccc", Broken.True )
                , ( "ddd", Broken.False )
                , ( "eee", Broken.Null )
                ]

        decoder =
            Json.Decode.map5 Something
                (Json.Decode.field "aaa" (Json.Decode.list Json.Decode.string))
                (Json.Decode.field "bbb" Json.Decode.float)
                (Json.Decode.field "ccc" Json.Decode.bool)
                (Json.Decode.field "ddd" Json.Decode.bool)
                (Json.Decode.field "eee" (Json.Decode.null ()))
    in
    describe "broken to elm/json"
        [ test "encode and decode" <|
            \_ ->
                Broken.encode value
                    |> Json.Decode.decodeValue decoder
                    |> Expect.equal
                        (Ok
                            { aaa = [ "foo", "bar" ]
                            , bbb = 1200
                            , ccc = True
                            , ddd = False
                            , eee = ()
                            }
                        )
        ]



-- Local Variables:
-- elm-format-elm-version: "0.19"
-- End:
