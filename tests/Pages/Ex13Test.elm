-- Ex13: Determining Compound Interest
--
-- - Prompt the user for principal amount, interest rate (as a percentage), number of years, and compounding frequency per year.
-- - Convert the interest rate by dividing it by 100.
-- - Use the compound interest formula to compute the final amount.
-- - Round up fractions of a cent to the next penny.
-- - Format the output as money.


module Pages.Ex13Test exposing (..)

import Expect exposing (Expectation)
import Pages.Ex13 as Ex13
import Test exposing (..)


suite : Test
suite =
    let
        update msg m =
            m |> Ex13.update msg |> Tuple.first
    in
    describe "Ex13 Module"
        [ describe "update"
            [ test "Change events should update the output" <|
                \_ ->
                    let
                        model =
                            Ex13.init
                                |> update (Ex13.PrincipalChanged "1500")
                                |> update (Ex13.RateChanged "4.3")
                                |> update (Ex13.YearsChanged "6")
                                |> update (Ex13.TimesChanged "4")
                    in
                    expectValidAmount model "1938.84"
            ]
        ]


expectValidAmount : Ex13.Model -> String -> Expectation
expectValidAmount model expected =
    case model.output of
        Ok (Just message) ->
            Expect.equal True <| String.contains expected message

        _ ->
            Expect.fail "unreachable: output should be Ok with a message"
