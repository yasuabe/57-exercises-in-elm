module Pages.Ex03Test exposing (..)

import Expect
import Pages.Ex03 as Ex03
import Test exposing (..)


suite : Test
suite =
    describe "Ex03 Module"
        [ describe "init"
            [ test "should initialize with empty quote, author and message" <|
                \_ ->
                    let
                        model =
                            Ex03.init
                    in
                    Expect.all
                        [ \m -> Expect.equal "" m.quote
                        , \m -> Expect.equal "" m.author
                        , \m -> Expect.equal "" m.message
                        ]
                        model
            ]
        , describe "update"
            [ test "QuoteChanged should update quote field" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            Ex03.update (Ex03.QuoteChanged "q001") <| Ex03.init
                    in
                    Expect.equal "q001" newModel.quote
            , test "AuthorChanged should update author field" <|
                \_ ->
                    let
                        ( newModel, _ ) =
                            Ex03.update (Ex03.AuthorChanged "a001") <| Ex03.init
                    in
                    Expect.equal "a001" newModel.author
            , test "Submit with non-empty quote and author should update output" <|
                \_ -> testSubmit "q002" "a002" "a002 says, \"q002\""
            , test "Submit with empty quote should set output empty" <|
                \_ -> testSubmit "" "a003" ""
            , test "Submit with empty author should set output empty" <|
                \_ -> testSubmit "q004" "" ""
            ]
        ]


testSubmit : String -> String -> String -> Expect.Expectation
testSubmit quote author expect =
    let
        model =
            { quote = quote, author = author, message = "---" }

        ( newModel, _ ) =
            Ex03.update Ex03.Submit model
    in
    Expect.equal expect newModel.message
