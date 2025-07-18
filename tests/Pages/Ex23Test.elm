module Pages.Ex23Test exposing (..)

import Expect
import Pages.Ex23 as Ex23
import Test exposing (..)


suite : Test
suite =
    describe "Ex23 Module"
        [ describe "traverse"
            [ test "returns the root element in the decisionTree for empty decision list" <|
                \_ ->
                    Expect.equal (Ex23.traverse []) <|
                        { steps = []
                        , lastStep = Ex23.Question "Is the car silent when you turn the key? "
                        }
            , test "returns the correct Diagnosis for a given decision list" <|
                \_ ->
                    Expect.equal (Ex23.traverse [ True, True ]) <|
                        { steps =
                            [ ( "Is the car silent when you turn the key? ", True )
                            , ( "Are the battery terminals corroded? ", True )
                            ]
                        , lastStep = Ex23.Solution "Clean terminals and try starting again."
                        }
            ]
        ]
