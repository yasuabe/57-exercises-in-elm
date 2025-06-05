module Pages.Ex01Test exposing (..)

import Expect
import Pages.Ex01 as Ex01
import Test exposing (..)


suite : Test
suite =
    describe "Ex01 Module"
        [ describe "init"
            [ test "should initialize with empty input and message" <|
                \_ ->
                    let
                        model = Ex01.init
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
                        model = Ex01.init
                        (newModel, _) = Ex01.update (Ex01.InputChanged "test") model
                    in
                    Expect.equal "test" newModel.input
            
            , test "Submit with non-empty input should set greeting message" <|
                \_ ->
                    let
                        model = { input = "Alice", message = "" }
                        (newModel, _) = Ex01.update Ex01.Submit model
                    in
                    Expect.equal "Hello, Alice, nice to meet you!" newModel.message
            
            , test "Submit with empty input should clear message" <|
                \_ ->
                    let
                        model = { input = "", message = "old message" }
                        (newModel, _) = Ex01.update Ex01.Submit model
                    in
                    Expect.equal "" newModel.message
            ]
        ]