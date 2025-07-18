module Pages.Ex23Test exposing (..)

import Expect exposing (Expectation)
import Pages.Ex23 as Ex23
import Test exposing (..)


suite : Test
suite =
    let
        update msg m =
            m |> Ex23.update msg |> Tuple.first
    in
    describe "Ex23 Module"
        [ describe "traverse"
            [ test "returns topmost element in the decisionTree for empty decision list" <|
                \_ ->
                    let
                        expected =
                                { steps = []
                                , lastStep = Ex23.Question "Is the car silent when you turn the key? "
                                }
                    in
                    Expect.equal expected <| Ex23.traverse []
            , test "returns Disgnosis according to given decision list" <|
                \_ ->
                    let
                        expected =
                                { steps =
                                    [ ("Is the car silent when you turn the key? ", True)
                                    , ("Are the battery terminals corroded? ", True)
                                    ]
                                , lastStep = Ex23.Solution "Clean terminals and try starting again."
                                }
                    in
                    Expect.equal expected <| Ex23.traverse [ True, True ]
            ]
        ]
