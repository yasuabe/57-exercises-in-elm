module Pages.Ex31Test exposing (..)

import Expect
import List exposing (head)
import List.Extra exposing (last)
import Pages.Ex31 exposing (calcHeartRate, generateTable)
import Test exposing (..)


suite : Test
suite =
    describe "Ex31 Module"
        [ describe "calcHeartRate"
            [ test "should calculate target heart rate" <|
                Expect.all
                    [ always <| Expect.equal 138 <| round <| calcHeartRate 65 22 55
                    , always <| Expect.equal 185 <| round <| calcHeartRate 65 22 90
                    , always <| Expect.equal 191 <| round <| calcHeartRate 65 22 95
                    ]
            ]
        , describe "generateTable"
            [ test "should generate a list of target heart rates for different intensities" <|
                \_ ->
                    generateTable 65 22
                        |> List.map (Tuple.mapBoth round round)
                        |> Expect.all
                            [ \result -> Expect.equal 9 <| List.length result
                            , \result -> Expect.equal (Just ( 55, 138 )) <| head result
                            , \result -> Expect.equal (Just ( 95, 191 )) <| last result
                            ]
            ]
        ]
