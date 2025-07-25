-- # Ex52: Creating Your Own Time Service
--
-- - Build a minimal web server that returns the current time as JSON: { "currentTime": "2050-01-24 15:06:26" }.
-- - Build a client that fetches this JSON, parses it, and displays the time in a readable format.
-- - Server must set Content-Type: application/json.
-- - Keep server code minimal.


port module Pages.Ex52 exposing (Model, Msg(..), init, subscriptions, update, view)

import Common.Format exposing (..)
import Common.ResultEx exposing (either)
import Common.TimeEx exposing (decompose, toEnglishMonth)
import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)
import Iso8601
import Json.Decode as Decode exposing (Decoder, decodeString)
import String exposing (fromInt)
import Time exposing (Posix)


port requestTime : () -> Cmd msg



-- MODEL


type alias CurrentTime =
    { currentTime : Posix
    }


type alias Model =
    { timestamp : Result String Posix
    }


init : Model
init =
    { timestamp = Err "The current time has not been retrieved yet." }



-- MSG


type Msg
    = Submit
    | TimeReceived String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
            ( model, requestTime () )

        TimeReceived t ->
            let
                a =
                    case decodeString currentTimeDecoder t of
                        Ok time ->
                            Ok time.currentTime

                        Err err ->
                            Err (Decode.errorToString err)
            in
            ( { model | timestamp = a }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        outputText =
            either formatPosix identity model.timestamp
    in
    div []
        [ div [ class "inputline" ] [ a [ href "timeserver.html", target "_blank", class "button-like" ] [ text "Time Service" ] ]
        , div [ class "inputline" ] [ button [ onClick Submit, class "button-like" ] [ text "Request Current Time" ] ]
        , div [ class "output" ] [ text outputText ]
        ]


formatPosix : Posix -> String
formatPosix posix =
    let
        ( ( year, month, day ), ( hour, minute, second ) ) =
            decompose Time.utc posix
    in
    "The current time is "
        ++ toTimeString hour minute second
        ++ " UTC "
        ++ toEnglishMonth month
        ++ " "
        ++ fromInt day
        ++ ", "
        ++ fromInt year
        ++ "."



-- HELPER FUNCTIONS


currentTimeDecoder : Decoder CurrentTime
currentTimeDecoder =
    Decode.map CurrentTime
        (Decode.field "currentTime" Iso8601.decoder)
