module Pages.Ex14Test exposing (..)

import Common.ResultMaybe exposing (ResultMaybe)
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
                always
                    (Ex14.init
                        |> callUpdate (Ex14.AmountChanged "10")
                        |> callUpdate (Ex14.StateChanged "WI")
                        |> Ex14.makeOutput
                        |> expectValidOutput [ "10.00", "0.55", "10.55" ] []
                    )
            , test "Sales tax should not be added outside WI" <|
                always
                    (Ex14.init
                        |> callUpdate (Ex14.AmountChanged "10")
                        |> callUpdate (Ex14.StateChanged "MN")
                        |> Ex14.makeOutput
                        |> expectValidOutput [ "10.00" ] [ "subtotal", "tax" ]
                    )
            ]
        ]


expectValidOutput : List String -> List String -> ResultMaybe String String -> Expectation
expectValidOutput included excluded actual =
    case actual of
        Ok (Just message) ->
            Expect.all
                [ \m -> Expect.equal True <| List.all (\w -> String.contains w m) included
                , \m -> Expect.equal True <| List.all (\w -> not <| String.contains w m) excluded
                ]
                message

        _ ->
            Expect.fail "unreachable: output should be Ok with a message"
