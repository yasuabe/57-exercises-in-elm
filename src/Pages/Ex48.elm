module Pages.Ex48 exposing (Model, Msg(..), init, kelvinToFahrenheit, update, view)

import Basics.Extra exposing (flip)
import Common.Events exposing (onEnter)
import Common.HttpEx exposing (errorToString)
import Common.Math exposing (roundToDecimals)
import Common.ResultMaybe as RM exposing (ResultMaybe)
import Common.SessionStorage exposing (getItem, setItem)
import Common.UI exposing (viewOutputBlock)
import Dict
import Html exposing (Html, button, div, fieldset, input, label, legend, pre, text)
import Html.Attributes exposing (checked, class, for, id, name, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import List



-- MODEL


type TempScale
    = C
    | F
    | K


type alias WeatherData =
    { temp : Float
    , location : String
    }


type alias Model =
    { apiKey : Maybe String
    , query : String
    , tmpApiKey : String
    , weatherData : ResultMaybe String WeatherData
    , scale : TempScale
    }


init : Model
init =
    { apiKey = Nothing
    , query = ""
    , tmpApiKey = "<API_KEY>"
    , weatherData = Ok Nothing
    , scale = C
    }


kelvinToFahrenheit : Float -> Float
kelvinToFahrenheit kelvin =
    (kelvin - 273.15) * 9 / 5 + 32 |> roundToDecimals 1


kelvinToCelsius : Float -> Float
kelvinToCelsius kelvin =
    kelvin - 273.15 |> roundToDecimals 1


toTempScale : String -> TempScale
toTempScale scale =
    [ ( "celsius", C ), ( "fahrenheit", F ), ( "kelvin", K ) ]
        |> Dict.fromList
        |> Dict.get scale
        |> Maybe.withDefault C


getScaleInfo : TempScale -> { id : String, text : String, func : Float -> Float }
getScaleInfo scale =
    case scale of
        C ->
            { id = "celsius", text = "Celsius", func = kelvinToCelsius }

        F ->
            { id = "fahrenheit", text = "Fahrenheit", func = kelvinToFahrenheit }

        K ->
            { id = "kelvin", text = "Kelvin", func = roundToDecimals 1 }



-- MSG


type Msg
    = LoadConfig
    | FetchData
    | UpdateLocation String
    | SessionStorageItemReceived D.Value
    | GotData (Result String WeatherData)
    | ScaleChanged String
    | ApiKeyChanged String
    | RegisterApiKey



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadConfig ->
            ( model, getItem "Ex53" )

        SessionStorageItemReceived value ->
            -- TODO: use `withNone`
            ( { model | apiKey = decodeConfig value }, Cmd.none )

        FetchData ->
            ( model, fetchWeatherData model )

        UpdateLocation input ->
            ( { model | query = String.trim input }, Cmd.none )

        GotData response ->
            ( { model | weatherData = Result.map Just response }, Cmd.none )

        ScaleChanged scale ->
            ( { model | scale = toTempScale scale }, Cmd.none )

        ApiKeyChanged apiKey ->
            ( { model | tmpApiKey = apiKey }, Cmd.none )

        RegisterApiKey ->
            ( { model | apiKey = Just model.tmpApiKey }, registerApiKey model.tmpApiKey )


decodeConfig : D.Value -> Maybe String
decodeConfig =
    D.decodeValue (D.field "apiKey" D.string)
        >> Result.mapError (D.errorToString >> Debug.log "Error decoding apiKey: ")
        >> Result.toMaybe


registerApiKey : String -> Cmd Msg
registerApiKey apiKey =
    case apiKey of
        "" ->
            Cmd.none

        key ->
            setItem ( "Ex53", E.object [ ( "apiKey", E.string key ) ] )



-- HTTP


fetchWeatherData : Model -> Cmd Msg
fetchWeatherData { apiKey, query } =
    case apiKey of
        Just key ->
            Http.get
                { url = "https://api.openweathermap.org/data/2.5/weather?q=" ++ query ++ "&appid=" ++ key
                , expect = Http.expectJson (Result.mapError errorToString >> GotData) weatherDataDecoder
                }

        Nothing ->
            Cmd.none


weatherDataDecoder : D.Decoder WeatherData
weatherDataDecoder =
    D.map2 WeatherData
        (D.field "main" (D.field "temp" D.float))
        (D.field "name" D.string)



-- VIEW


viewWeatherQuery : String -> List (Html Msg)
viewWeatherQuery key =
    [ div [ class "input-line" ]
        [ label [ for "query" ] [ text "Where Are You? " ]
        , input
            [ id "query"
            , class "inputline__text"
            , placeholder "eg. Kanagawa"
            , onInput UpdateLocation
            , onEnter (always FetchData)
            ]
            [ text "Fetch" ]
        , button [ class "inputline__button", onClick FetchData ] [ text "Fetch" ]
        , div [ style "display" "none" ] [ text key ]
        ]
    ]


viewApiKeyInput : List (Html Msg)
viewApiKeyInput =
    [ pre
        [ class "error-message" ]
        [ text "OpenWeatherMap API key is missing. \nPlease set it in session storage." ]
    , div []
        [ input
            [ type_ "text"
            , class "ex48__apikey-input"
            , placeholder "Enter API key for OpenWeatherMap"
            , onInput ApiKeyChanged
            ]
            []
        , button
            [ type_ "text"
            , onClick RegisterApiKey
            ]
            [ text "Set API key" ]
        ]
    ]


viewInputBlock : Maybe String -> Html Msg
viewInputBlock apiKey =
    Maybe.map viewWeatherQuery apiKey
        |> Maybe.withDefault viewApiKeyInput
        |> div [ class "inputblock" ]


viewScaleRadio : TempScale -> List (Html Msg)
viewScaleRadio scale =
    let
        makeScaleRadio key =
            let
                v =
                    getScaleInfo key
            in
            [ input
                [ type_ "radio"
                , name "scale"
                , id v.id
                , value v.id
                , onInput ScaleChanged
                , checked (scale == key)
                ]
                []
            , label [ for v.id ] [ text v.text ]
            ]
    in
    [ fieldset
        []
        [ legend [] [ text "Temperature Scale" ]
        , div [ class "scale-radio" ] (List.concatMap makeScaleRadio [ C, F, K ])
        ]
    ]


viewFetchResult : Model -> Html Msg
viewFetchResult { weatherData, scale } =
    let
        scaleInfo =
            getScaleInfo scale

        outputBlock =
            weatherData
                |> Result.mapError List.singleton
                |> RM.map
                    (\w ->
                        String.join "\n"
                            [ w.location ++ " weather: "
                            , String.fromFloat (scaleInfo.func w.temp) ++ " degrees " ++ scaleInfo.text
                            ]
                    )
                |> flip viewOutputBlock "No data available"
                |> List.singleton
    in
    div []
        (viewScaleRadio scale ++ outputBlock)


view : Model -> Html Msg
view model =
    div [ style "max-width" "600px" ]
        [ viewInputBlock model.apiKey
        , viewFetchResult model
        ]
