module Common.MaybeExTest exposing (..)

import Common.MaybeEx exposing(mapToList, filter)
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
                    Expect.equal [] <| mapToList (always [])Nothing
            , test "should map `Just x` to a non-empty list by applying `f` to `x`" <|
                \_ ->
                    Expect.equal ["1", "2"] <| mapToList (range 1 >> map fromInt) <| Just 2
            ]
        , describe "filter"
            [ test "should map `Nothing` to `Nothing`" <|
                \_ ->
                    Expect.equal Nothing <| filter (always True) Nothing
            , test "should keep `Just x` if x meets given predicate" <|
                \_ ->
                    Expect.equal (Just 1) <| filter ((<) 0) <| Just 1
            , test "should map `Just x` to `Nothing` if x does not meet the given predicate" <|
                \_ ->
                    Expect.equal Nothing <| filter ((>) 0) <| Just 1
            ]
        ]
