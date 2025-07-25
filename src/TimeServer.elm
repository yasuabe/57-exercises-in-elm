port module TimeServer exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Iso8601
import Json.Encode as E
import Task
import Time exposing (Posix, Zone, utc)



-- exposing (Value)


port responseTime : String -> Cmd msg


port timeRequested : (() -> msg) -> Sub msg



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { zone : Zone
    , time : Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model utc (Time.millisToPosix 0)
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = AdjustTimeZone Zone
    | Accept ()
    | CurrentTime Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )

        Accept _ ->
            ( model
            , responseTime <|
                E.encode 0 <|
                    E.object [ ( "currentTime", E.string <| Iso8601.fromTime model.time ) ]
            )

        CurrentTime time ->
            ( { model | time = time }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ timeRequested Accept
        , Time.every 1000 CurrentTime
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Time Server" ]
        , p [] [ text <| Iso8601.fromTime model.time ]
        ]



-- HELPER FUNCTIONS
