module Common.MathTest exposing (..)

import Common.Math exposing (roundToDecimals)
import Expect exposing (..)
import Test exposing (..)
import Utils.ExpectEx exposing (equateFloats)


suite : Test
suite =
    describe "Common.Math"
        [ describe "roundToDecimals"
            [ test "Rounds to 2 decimal places" <|
                \_ ->
                    roundToDecimals 2 3.14159
                        |> equateFloats 3.14
            , test "Rounds to 0 decimal places (integer)" <|
                \_ ->
                    roundToDecimals 0 3.7
                        |> equateFloats 4.0
            , test "Rounds to 3 decimal places" <|
                \_ ->
                    roundToDecimals 3 1.23456
                        |> equateFloats 1.235
            , test "Rounds negative numbers" <|
                \_ ->
                    roundToDecimals 2 -2.567
                        |> equateFloats -2.57
            , test "Already at specified decimal places" <|
                \_ ->
                    roundToDecimals 2 5.12
                        |> equateFloats 5.12
            , test "Rounds 0" <|
                \_ ->
                    roundToDecimals 0 0.0
                        |> equateFloats 0.0
            ]
        ]
