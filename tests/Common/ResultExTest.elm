module Common.ResultExTest exposing (..)

import Common.ResultEx as SUT
import Expect exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Common.ResultEx"
        [ describe "either"
            [ test "should map both sides to values of the same type" <|
                \_ ->
                    let
                        f =
                            SUT.either (\c -> "Char: " ++ String.fromChar c) (\i -> "Int: " ++ String.fromInt i)
                    in
                    Expect.all
                        [ \_ -> f (Ok 'a') |> equal "Char: a"
                        , \_ -> f (Err 42) |> equal "Int: 42"
                        ]
                        ()
            ]
        ]
