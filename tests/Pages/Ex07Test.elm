module Pages.Ex07Test exposing (..)

import Expect
import Pages.Ex07 as Ex07
import Test exposing (..)


suite : Test
suite =
    describe "Ex07 Module"
        [ describe "init"
            [ test "should initialize with empty length, width and area" <|
                \_ ->
                    let
                        model =
                            Ex07.init
                    in
                    Expect.all
                        [ \m -> Expect.equal "" m.length
                        , \m -> Expect.equal "" m.width
                        , \m -> Expect.equal (Ok Nothing) m.output
                        ]
                        model
            ]
        , describe "update"
            [ test "Change events should update the output" <|
                \_ ->
                    let
                        ( model, _ ) =
                            Ex07.init
                                |> Ex07.update (Ex07.LengthChanged "15")
                                |> Tuple.first
                                |> Ex07.update (Ex07.WidthChanged "20")
                    in
                    expectValidArea model "300" "27.87"
            ]
        ]


expectValidArea : Ex07.Model -> String -> String -> Expect.Expectation
expectValidArea model areaInFeet areaInMeters =
    case model.output of
        Ok (Just message) ->
            Expect.all
                [ \m -> Expect.equal True <| String.contains areaInFeet m -- TODO: use common test utility function
                , \m -> Expect.equal True <| String.contains areaInMeters m
                ]
                message

        _ ->
            Expect.fail "unreachable: output should be Ok with a message"
