module Pages.Ex28Test exposing (..)

import Array exposing (toList)
import Expect
import Pages.Ex28 as Ex28 exposing (Model, Msg(..))
import Test exposing (..)


suite : Test
suite =
    describe "Ex28"
        [ describe "update function behavior"
            [ test "calculates the total of provided inputs, excluding non-number strings" <|
                \_ ->
                    Ex28.init
                        |> applyEvents
                            [ NumberChanged 0 ""
                            , NumberChanged 1 " 2"
                            , NumberChanged 2 "3 "
                            , NumberChanged 3 "not a number"
                            , NumberChanged 4 "5"
                            ]
                        |> Expect.all
                            [ Ex28.calculateTotal >> Expect.equal 10
                            , Expect.equalLists
                                [ Ok Nothing
                                , Ok (Just 2)
                                , Ok (Just 3)
                                , Err "not a number"
                                , Ok (Just 5)
                                ]
                            ]
            ]
        ]


applyEvents : List Msg -> Model -> Model
applyEvents messages model =
    List.foldl (\msg m -> Ex28.update msg m |> Tuple.first) model messages
