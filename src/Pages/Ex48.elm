module Pages.Ex48 exposing (Model, Msg(..), init, update, view)

import Common.ResultEx as ResultEx
import Common.SessionStorage exposing (getItem)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Json.Encode as E



-- MODEL


type alias Model =
    { apiKey : String
    , temp : Float
    }


init : Model
init =
    { apiKey = ""
    , temp = 0.0
    }



-- MSG


type Msg
    = LoadConfig
    | FetchData
    | SessionStorageItemReceived D.Value
    | GotData (Result String Float)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadConfig ->
            ( model, getItem "Ex53" )

        FetchData ->
            ( model, fetchWeatherData model )

        SessionStorageItemReceived value ->
            ( { model | apiKey = decodeConfig value }, Cmd.none )

        GotData (Ok temp) ->
            ( { model | temp = temp }, Cmd.none )

        GotData (Err error) ->
            always ( model, Cmd.none ) <| Debug.log ("Error fetching data: " ++ error) ( model, Cmd.none )


decodeConfig : D.Value -> String
decodeConfig =
    D.decodeValue (D.field "apiKey" D.string)
        >> Result.mapError (\e -> "Error decoding apiKey: " ++ D.errorToString e)
        >> ResultEx.fromEither



-- HTTP


fetchWeatherData : Model -> Cmd Msg
fetchWeatherData { apiKey, temp } =
    Http.get
        { url = "https://api.openweathermap.org/data/2.5/weather?q=Kanagawa&appid=" ++ apiKey
        , expect = Http.expectJson (\x -> GotData <| Result.mapError (always "lsdfasdf") x) weatherDataDecoder
        }


weatherDataDecoder : D.Decoder Float
weatherDataDecoder =
    D.field "main" (D.field "temp" D.float)


-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "not implemented" ]
        , div [ style "border" "1px solid black" ] [ text model.apiKey ]
        , div [ style "border" "1px solid black" ] [ text <| String.fromFloat model.temp ++ " K" ]
        , button [ onClick FetchData ] [ text "Submit" ]
        ]
