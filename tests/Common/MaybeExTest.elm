module Common.MaybeExTest exposing (..)

import Common.MaybeEx exposing(..)
import Expect exposing (..)
import Test exposing (..)
import List exposing (range, map)
import String exposing (fromInt)


suite : Test
suite =
    describe "Common.MaybeEx"
        [ describe "mapToList"
            [ test "should map `Nothing` to an empty list" <|
                \_ ->
                    Expect.equal [] <| Common.MaybeEx.mapToList (always [])Nothing
            , test "should map `Just x` to a non-empty list by applying `f` to `x`" <|
                \_ ->
                    Expect.equal ["1", "2"] <| mapToList (range 1 >> map fromInt) <| Just 2
            ]
        ]
