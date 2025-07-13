module Pages.Ex13Test exposing (..)

import Expect
import Pages.Ex13 as Ex13
import Test exposing (..)
import Debug exposing (log)


suite : Test
suite =
    describe "Ex13 Module"
        [ describe "update"
            [ test "Change events should update the output" <|
                \_ ->
                    let
                        ( model, _ ) =
                            Ex13.init
                                |> Ex13.update (Ex13.PrincipalChanged "1500")
                                |> Tuple.first
                                |> Ex13.update (Ex13.RateChanged "4.3")
                                |> Tuple.first
                                |> Ex13.update (Ex13.YearsChanged "6")
                                |> Tuple.first
                                |> Ex13.update (Ex13.TimesChanged "4")
                    in
                    expectValidArea model "1938.84"
            ]
        ]


expectValidArea : Ex13.Model -> String -> Expect.Expectation -- TODO: rename
expectValidArea model areaInFeet =
    case model.output of
        Ok (Just message) ->
            Expect.all
                [ \m -> Expect.equal True <| String.contains areaInFeet m
                ]
                (log "message: " message)

        _ ->
            Expect.fail "unreachable: output should be Ok with a message"
