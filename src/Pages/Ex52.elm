-- # Ex52: Creating Your Own Time Service
--
-- - Build a minimal web server that returns the current time as JSON: { "currentTime": "2050-01-24 15:06:26" }.
-- - Build a client that fetches this JSON, parses it, and displays the time in a readable format.
-- - Server must set Content-Type: application/json.
-- - Keep server code minimal.


port module Pages.Ex52 exposing (Model, Msg(..), init, subscriptions, update, view)

import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)


port requestTime : () -> Cmd msg



-- MODEL


type alias Model =
    { timestamp : String
    }


init : Model
init =
    { timestamp = "" }



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
            ( { model | timestamp = t }, Cmd.none )



-- SUBSCRIPTIONS
-- TODO: is this needed? If so, implement it to listen for IndexedDB changes or other events.


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ a [ href "timeserver.html", target "_blank", class "button-like" ] [ text "Time Service" ]
        , div [ class "inputline" ] [ button [ onClick Submit, class "button-like" ] [ text "Request Current Time" ] ]
        , div [ class "output" ] [ text <| "timestamp: " ++ model.timestamp ]
        ]
