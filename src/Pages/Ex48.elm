module Pages.Ex48 exposing (Model, Msg(..), init, kelvinToFahrenheit, update, view)

import Common.Events exposing (submitOnEnter)
import Common.HttpEx exposing (errorToString)
import Common.Math exposing (roundToDecimals)
import Common.ResultMaybe as RM exposing (ResultMaybe)
import Common.SessionStorage exposing (getItem)
import Common.UI exposing (viewOutputBlock)
import Dict exposing (Dict)
import Html exposing (Html, button, div, fieldset, input, label, legend, text)
import Html.Attributes exposing (checked, class, for, id, name, placeholder, style, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Http
import Json.Decode as D
import List



-- MODEL


type alias WeatherData =
    { temp : Float
    , location : String
    }


type alias Model =
    { apiKey : Maybe String
    , input : String
    , weatherData : ResultMaybe String WeatherData
    , scale : String
    }


init : Model
init =
    { apiKey = Nothing
    , input = ""
    , weatherData = Ok Nothing
    , scale = "C"
    }


kelvinToFahrenheit : Float -> Float
kelvinToFahrenheit kelvin =
    (kelvin - 273.15) * 9 / 5 + 32 |> roundToDecimals 1


kelvinToCelsius : Float -> Float
kelvinToCelsius kelvin =
    kelvin - 273.15 |> roundToDecimals 1


tempScaleDict : Dict String { id : String, text : String, func : Float -> Float }
tempScaleDict =
    Dict.fromList
        [ ( "C", { id = "celsius", text = "Celsius", func = kelvinToCelsius } )
        , ( "F", { id = "fahrenheit", text = "Fahrenheit", func = kelvinToFahrenheit } )
        , ( "K", { id = "kelvin", text = "Kelvin", func = roundToDecimals 1 } )
        ]



-- MSG


type Msg
    = LoadConfig
    | FetchData
    | UpdateLocation String
    | SessionStorageItemReceived D.Value
    | GotData (Result String WeatherData)
    | ScaleChanged String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadConfig ->
            ( model, getItem "Ex53" )

        SessionStorageItemReceived value ->
            ( { model | apiKey = decodeConfig value }, Cmd.none )

        FetchData ->
            ( model, fetchWeatherData model )

        UpdateLocation input ->
            ( { model | input = String.trim input }, Cmd.none )

        GotData response ->
            ( { model | weatherData = Result.map Just response }, Cmd.none )

        ScaleChanged scale ->
            always ( { model | scale = scale }, Cmd.none ) (Debug.log "Scale changed to: " scale)


decodeConfig : D.Value -> Maybe String
decodeConfig =
    D.decodeValue (D.field "apiKey" D.string)
        >> Result.mapError (\e -> Debug.log "Error decoding apiKey: " (D.errorToString e))
        >> Result.toMaybe



-- HTTP


fetchWeatherData : Model -> Cmd Msg
fetchWeatherData { apiKey, input } =
    case apiKey of
        Just key ->
            Http.get
                { url = "https://api.openweathermap.org/data/2.5/weather?q=" ++ input ++ "&appid=" ++ key
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


view : Model -> Html Msg
view model =
    div [ style "max-width" "600px" ]
        [ viewInputBlock model.apiKey
        , viewFetchResult model
        ]


viewInputBlock : Maybe String -> Html Msg
viewInputBlock apiKey =
    case apiKey of
        Just key ->
            div
                [ class "inputblock" ]
                [ div [ class "input-line" ]
                    [ label [ for "query" ] [ text "Where Are You? " ]
                    , input
                        [ id "query"
                        , class "inputline__text"
                        , placeholder "eg. Kanagawa"
                        , onInput UpdateLocation
                        , on "keydown" <| submitOnEnter FetchData
                        ]
                        [ text "Fetch" ]
                    , button [ class "inputline__button", onClick FetchData ] [ text "Fetch" ]
                    , div [ style "display" "none" ] [ text key ]
                    ]
                ]

        Nothing ->
            div [ class "error-message" ] [ text "API key is missing. Please set it in session storage." ]


viewFetchResult : Model -> Html Msg
viewFetchResult { weatherData, scale } =
    let
        tempScale =
            Dict.get scale tempScaleDict |> Maybe.withDefault { id = "unknown", text = "Unknown", func = roundToDecimals 1 }

        output : ResultMaybe (List String) String
        output =
            Result.mapError List.singleton weatherData
                |> RM.map
                    (\w ->
                        String.join "\n"
                            [ w.location ++ " weather: "
                            , String.fromFloat (tempScale.func w.temp) ++ " degrees " ++ tempScale.text
                            ]
                    )

        options =
            List.concatMap
                (\( key, v ) ->
                    [ input [ type_ "radio", name "scale", id v.id, value key, onInput ScaleChanged, checked (scale == key) ] []
                    , label [ for v.id ] [ text v.text ]
                    ]
                )
                (Dict.toList tempScaleDict)

        scaleRadio : List (Html Msg)
        scaleRadio =
            case weatherData of
                Ok (Just _) ->
                    [ fieldset
                        []
                        [ legend [] [ text "temperature scales" ]
                        , div [ class "scale-radio" ] options
                        ]
                    ]

                _ ->
                    []
    in
    div [] (scaleRadio ++ [ viewOutputBlock output "No data available" ])
