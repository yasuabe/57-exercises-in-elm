module Pages.Ex14Test exposing (..)

import Common.ResultMaybe exposing (ResultMaybe)
import Debug exposing (log)
import Expect exposing (Expectation)
import Pages.Ex14 as Ex14
import Test exposing (..)


suite : Test
suite =
    let
        callUpdate msg model =
            Ex14.update msg model |> Tuple.first
    in
    describe "Ex14 Module"
        [ describe "update"
            [ test "Sales tax should be added in WI" <|
                \_ ->
                    let
                        model =
                            Ex14.init
                                |> callUpdate (Ex14.AmountChanged "10")
                                |> callUpdate (Ex14.StateChanged "WI")
                    in
                    expectValidOutput model.output [ "10.00", "0.55", "10.55" ] []
            , test "Sales tax should not be added outside WI" <|
                \_ ->
                    let
                        model =
                            Ex14.init
                                |> callUpdate (Ex14.AmountChanged "10")
                                |> callUpdate (Ex14.StateChanged "MN")
                    in
                    expectValidOutput model.output [ "10.00" ] [ "subtotal", "tax" ]
            ]
        ]


expectValidOutput : ResultMaybe (List String) String -> List String -> List String -> Expectation
expectValidOutput output included excluded =
    case output of
        Ok (Just message) ->
            Expect.all
                [ \m -> Expect.equal True <| List.all (\w -> String.contains w m) included
                , \m -> Expect.equal True <| List.all (\w -> not <| String.contains w m) excluded
                ]
                (log "message: " message)

        _ ->
            Expect.fail "unreachable: output should be Ok with a message"
