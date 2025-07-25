module Pages.Ex52Test exposing (..)

import Common.ResultEx as ResultEx
import Expect
import Iso8601 exposing (fromTime)
import Pages.Ex52 as Ex52
import Test exposing (..)
import Test.Html.Selector exposing (..)
import Tuple exposing (first)
import Utils.ExpectEx exposing (checkStringContains)


suite : Test
suite =
    let
        testUpdate : String -> String
        testUpdate str =
            Ex52.update (Ex52.TimeReceived str) Ex52.init
                |> first
                |> .timestamp
                |> Result.map fromTime
                |> ResultEx.fromEither
    in
    describe "Ex52 Module"
        [ describe "update"
            [ describe "on TimeReceived"
                [ test "should update the timestamp field with a valid time string" <|
                    \_ ->
                        testUpdate "{\"currentTime\": \"2023-10-01T12:00:00Z\"}"
                            |> Expect.equal "2023-10-01T12:00:00.000Z"
                , test "should indicate an error when given an invalid time string" <|
                    \_ ->
                        testUpdate "{\"currentTime\": \"invalid format\"}"
                            |> checkStringContains "Problem with the value at json.currentTime"
                , test "should indicate an error if the JSON lacks the 'currentTime' field" <|
                    \_ ->
                        testUpdate "{\"invalidFieldName\": \"2023-10-01T12:00:00Z\"}"
                            |> checkStringContains "Expecting an OBJECT with a field named `currentTime`"
                ]
            ]
        ]
