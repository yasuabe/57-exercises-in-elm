module Pages.Ex31 exposing (Model, Msg(..), calcHeartRate, generateTable, init, update, view)

import Basics.Extra as BX
import Common.CmdEx exposing (withNone)
import Common.ResultEx as RE
import Common.ResultMaybe as RM exposing (ResultMaybe, convertInputToIntField, map3)
import Common.UI exposing (IntField, intToFieldValue)
import Html exposing (Html, div, input, span, table, td, text, th, tr)
import Html.Attributes as HA exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onInput)



-- MODEL


type alias Rate =
    Float


type alias Model =
    { restingHR : IntField
    , age : IntField
    , intensity : IntField
    }


init : Model
init =
    { restingHR = Ok Nothing
    , age = Ok Nothing
    , intensity = Ok (Just 60)
    }


rateToString : Rate -> String
rateToString =
    round >> String.fromInt >> BX.flip (++) " bpm"



-- MSG


type Msg
    = RestingHRChanged String
    | AgeChanged String
    | IntensityChanged String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RestingHRChanged str ->
            withNone { model | restingHR = convertInputToIntField str }

        AgeChanged str ->
            withNone { model | age = convertInputToIntField str }

        IntensityChanged str ->
            withNone { model | intensity = convertInputToIntField str }


calcHeartRate : Float -> Float -> Float -> Float
calcHeartRate restingHR age intensity =
    let
        maxHR =
            220 - age

        reserveHR =
            maxHR - restingHR

        targetHR =
            reserveHR * intensity / 100 + restingHR
    in
    targetHR


generateTable : Float -> Float -> List ( Float, Float )
generateTable restingHR age =
    List.map
        (\intensity -> ( intensity, calcHeartRate restingHR age intensity ))
        (List.range 0 8 |> List.map (\i -> toFloat i * 5.0 + 55.0))



-- VIEW


viewInputField :
    String
    -> String
    -> (String -> msg)
    -> ResultMaybe String a
    -> (ResultMaybe String a -> String)
    -> Html msg
viewInputField title placeholder_ inputHandler modelValue toString =
    let
        backgroundColor =
            RE.either
                (always <| style "background-color" "inherit")
                (always <| class "error-message")
    in
    div [ class "inputline" ]
        [ span [ class "ex31__inputline-label" ] [ text title ]
        , input
            [ class "inputline__number ex31__inputline-input"
            , placeholder placeholder_
            , onInput inputHandler
            , value (toString modelValue)
            , backgroundColor modelValue
            ]
            []
        ]


viewIntField :
    String
    -> String
    -> (String -> msg)
    -> ResultMaybe String Int
    -> Html msg
viewIntField title placeholder_ inputHandler modelValue =
    viewInputField title placeholder_ inputHandler modelValue intToFieldValue


viewIntensityRange : (String -> msg) -> ResultMaybe String Int -> Html msg
viewIntensityRange inputHandler intensity =
    div [ class "inputline", style "width" "100%" ]
        [ span [ class "ex31__inputline-label" ] [ text "Intensity:" ]
        , div
            [ class "ex31__intensity-range-component" ]
            [ input
                [ type_ "range"
                , class "ex31__intensity-range"
                , onInput inputHandler
                , HA.min "55"
                , HA.max "95"
                , HA.step "5"
                , value (intToFieldValue intensity)
                ]
                []
            , span
                [ class "ex31__intensity" ]
                [ text (intToFieldValue intensity ++ "%") ]
            ]
        ]


viewHeartRateRow : String -> Html msg
viewHeartRateRow heartRate =
    div [ class "inputline", style "width" "100%" ]
        [ span [ class "ex31__inputline-label" ] [ text "Heart Rate:" ]
        , span [ class "ex31__heart-rate" ] [ text heartRate ]
        ]


viewTable : Model -> Html msg
viewTable model =
    let
        rmPairs =
            RM.map2 generateTable (RM.map toFloat model.restingHR) (RM.map toFloat model.age)

        makeTr ( i, r ) =
            tr [ class "ex31__tr" ]
                [ td [] [ text ((String.fromInt << round) i ++ "%") ]
                , td [] [ text (rateToString r) ]
                ]

        tableHeader =
            tr []
                [ th [ class "ex31__th-intensity" ] [ text "Intensity" ]
                , th [ class "ex31__th-rate" ] [ text "Rate" ]
                ]
    in
    case rmPairs of
        Ok (Just pairs) ->
            table [ class "ex31__table" ]
                [ Html.tbody [] (tableHeader :: List.map makeTr pairs) ]

        _ ->
            div [ style "display" "none" ] []


viewOutputArea : Model -> Html msg
viewOutputArea model =
    let
        result =
            map3 calcHeartRate
                (RM.map toFloat model.restingHR)
                (RM.map toFloat model.age)
                (RM.map toFloat model.intensity)

        rateLine =
            case result of
                Ok (Just rate) ->
                    viewHeartRateRow <| rateToString rate

                Ok Nothing ->
                    text "Please enter all values."

                Err _ ->
                    div [ class "output error-message" ]
                        [ text "Please fix the invalid input." ]
    in
    div [] [ rateLine, viewTable model ]


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ viewIntField "Resting Pulse:" "e.g. 65" RestingHRChanged model.restingHR
            , viewIntField "Age:" "e.g. 22" AgeChanged model.age
            , viewIntensityRange IntensityChanged model.intensity
            ]
        , div []
            [ viewOutputArea model ]
        ]
