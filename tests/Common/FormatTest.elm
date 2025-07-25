module Common.FormatTest exposing (..)

import Common.Format as SUT
import Expect exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Common.Format"
        [ describe "padZeros"
            [ test "should not add zeros if the number already has the required digits" <|
                \_ ->
                    Expect.all
                        [ \_ -> SUT.padZeros 1 50 |> equal "50"
                        , \_ -> SUT.padZeros 2 50 |> equal "50"
                        ]
                        ()
            , test "should add leading zeros to match the required digit count" <|
                \_ ->
                    Expect.all
                        [ \_ -> SUT.padZeros 3 50 |> equal "050"
                        ]
                        ()
            ]
        , describe "toTimeString"
            [ test "should format three numbers into a time string in HH:MM:SS format" <|
                \_ ->
                    Expect.all
                        [ \_ -> SUT.toTimeString 0 0 0 |> equal "00:00:00"
                        , \_ -> SUT.toTimeString 1 23 456 |> equal "01:23:56"
                        ]
                        ()
            ]
        ]
