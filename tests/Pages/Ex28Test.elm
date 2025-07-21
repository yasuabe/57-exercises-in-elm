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
                    let
                        actual =
                            Ex28.init
                                |> applyEvents
                                    [ NumberChanged 0 ""
                                    , NumberChanged 1 " 2"
                                    , NumberChanged 2 "3 "
                                    , NumberChanged 3 "not a number"
                                    , NumberChanged 4 "5"
                                    ]
                    in
                    Expect.all
                        [ \m -> Expect.equal 10 m.total
                        , \m ->
                            Expect.equalLists
                                [ "", "2", "3", "not a number", "5" ]
                                (toList m.inputStrings)
                        ]
                        actual
            ]
        ]


applyEvents : List Msg -> Model -> Model
applyEvents messages model =
    List.foldl (\msg m -> Ex28.update msg m |> Tuple.first) model messages
