module Pages.Ex26Test exposing (..)

import Expect
import Pages.Ex26 exposing (calculateMonthsUntilPaidOff)
import Test exposing (..)


suite : Test
suite =
    describe "Ex26 Module"
        [ describe "calculateMonthsUntilPaidOff "
            [ test "calculates 70 months for $5000 at 12% with $100 payments" <|
                \_ ->
                    Expect.equal (calculateMonthsUntilPaidOff 5000 0.12 100) 70
            ]
        ]
