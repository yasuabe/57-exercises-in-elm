module Common.USStatesTest exposing (..)

import Common.USStates exposing (findANSICode)
import Expect exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Common.USStates"
        [ describe "findANSICode"
            [ test "finds by mixed-case ANSI code including spaces" <|
                \_ -> Expect.equal (findANSICode " mN") (Just "MN")
            , test "finds by state name with extra spaces and punctuation" <|
                \_ -> Expect.equal (findANSICode " U .S. Virgin Islands ") (Just "VI")
            , test "returns Nothing when the input is not found" <|
                \_ -> Expect.equal (findANSICode " New Y0rk") Nothing
            ]
        ]
