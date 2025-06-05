module Pages.Ex02Test exposing (..)

import Expect
import Pages.Ex02 as Ex02
import Test exposing (..)


suite : Test
suite =
    describe "Ex02 Module"
        [ describe "init"
            [ test "should initialize with empty input and message" <|
                \_ ->
                    let
                        model = Ex02.init
                    in
                    Expect.all
                        [ \m -> Expect.equal "" m.input
                        , \m -> Expect.equal "" m.message
                        ] model
            ]
        , describe "update"
            [ test "InputChanged should update input field" <|
                \_ ->
                    let
                        model = Ex02.init
                        (newModel, _) = Ex02.update (Ex02.InputChanged "test input") model
                    in
                    Expect.equal "test input" newModel.input
            
            , test "Submit with non-empty input should report character count" <|
                \_ ->
                    let
                        model = { input = "Homer", message = "" }
                        (newModel, _) = Ex02.update Ex02.Submit model
                    in
                    Expect.equal "Homer has 5 characters." newModel.message
            ]
        ]