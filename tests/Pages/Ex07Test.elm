module Pages.Ex07Test exposing (..)

import Common.ResultMaybe exposing (ResultMaybe)
import Expect
import Pages.Ex07 as Ex07
import Test exposing (..)


suite : Test
suite =
    describe "Ex07 Module"
        [ describe "update"
            [ test "Change events should update the output" <|
                always <|
                    (Ex07.init
                        |> Ex07.update (Ex07.LengthChanged "15")
                        |> Tuple.first
                        |> Ex07.update (Ex07.WidthChanged "20")
                        |> Tuple.first
                        |> Ex07.makeOutput
                        |> expectValidArea "300" "27.87"
                    )
            ]
        ]


expectValidArea : String -> String -> ResultMaybe String String -> Expect.Expectation
expectValidArea areaInFeet areaInMeters actual =
    case actual of
        Ok (Just message) ->
            Expect.all
                [ Expect.equal True << String.contains areaInFeet
                , Expect.equal True << String.contains areaInMeters
                ]
                message

        _ ->
            Expect.fail "unreachable: output should be Ok with a message"
