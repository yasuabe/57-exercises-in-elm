module Common.ResultMaybeTest exposing (..)

import Common.ResultMaybe exposing (collectErrors)
import Expect exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Common.ResultMaybe"
        [ describe "collect errors"
            [ test "empty list yields empty lists" <|
                \_ ->
                    collectErrors []
                        |> equalLists []
            , test "collect error values from given List of ResultMaybe" <|
                \_ ->
                    collectErrors [Err 1, Ok (Just "a"), Err 2, Ok (Just "b")]
                        |> equalLists [1, 2]
            ]
        ]
