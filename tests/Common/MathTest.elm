module Common.MathTest exposing (..)

import Common.Math exposing (roundToDecimals)
import Expect exposing (..)
import Test exposing (..)
import Utils.ExpectEx exposing (equateFloats)


suite : Test
suite =
    describe "Common.Math"
        [ describe "roundToDecimals"
            [ test "小数点以下2桁に丸める" <|
                \_ ->
                    roundToDecimals 3.14159 2
                        |> equateFloats 3.14
            , test "小数点以下0桁（整数に丸める）" <|
                \_ ->
                    roundToDecimals 3.7 0
                        |> equateFloats 4.0
            , test "小数点以下3桁に丸める" <|
                \_ ->
                    roundToDecimals 1.23456 3
                        |> equateFloats 1.235
            , test "負の数を丸める" <|
                \_ ->
                    roundToDecimals -2.567 2
                        |> equateFloats -2.57
            , test "既に指定桁数の場合" <|
                \_ ->
                    roundToDecimals 5.12 2
                        |> equateFloats 5.12
            , test "0を丸める" <|
                \_ ->
                    roundToDecimals 0.0 2
                        |> equateFloats 0.0
            ]
        ]
