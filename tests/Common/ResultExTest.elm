module Common.ResultExTest exposing (..)

import Common.Format as Format
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
        , describe "fromEither"
            [ test "should extract the value of the same type from a `Result a a`" <|
                \_ ->
                    let
                        f =
                            SUT.fromEither
                    in
                    Expect.all
                        [ \_ -> f (Ok "ok") |> equal "ok"
                        , \_ -> f (Err "42") |> equal "42"
                        ]
                        ()
            ]
        , describe "mapBoth"
            [ test "should map a pair of functions to both side of `Result` value" <|
                \_ ->
                    let
                        f =
                            SUT.mapEither Format.toTwoDigits String.length
                    in
                    Expect.all
                        [ \_ -> f (Ok 7) |> equal (Ok "07")
                        , \_ -> f (Err "42") |> equal (Err 2)
                        ]
                        ()
            ]
        ]
