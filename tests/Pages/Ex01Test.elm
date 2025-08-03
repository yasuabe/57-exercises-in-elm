module Pages.Ex01Test exposing (..)

import Expect
import Pages.Ex01 as Ex01
import Test exposing (..)


suite : Test
suite =
    describe "Ex01 Module"
        [ describe "update"
            [ test "InputChanged should update input field" <|
                always (
                    Ex01.init
                        |> Ex01.update (Ex01.Submit "test")
                        |> Tuple.first
                        |> Ex01.makeGreeting
                        |> Expect.equal (Just "Hello, test, nice to meet you!")
                )
            ]
        ]