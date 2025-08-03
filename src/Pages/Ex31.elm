module Pages.Ex31 exposing (Model, Msg(..), calcHeartRate, generateTable, init, update, view)

import Basics.Extra as BX
import Common.ResultEx as RE
import Common.ResultMaybe as RM exposing (ResultMaybe, map3)
import Html exposing (Html, div, input, span, table, td, text, th, tr)
import Html.Attributes as HA exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onInput)
import Maybe.Extra as MX
import Result.Extra as RX



-- MODEL


type alias Rate =
    Float


type alias Model =
    { restingHR : ResultMaybe String Int
    , age : ResultMaybe String Int
    , intensity : ResultMaybe String Int
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


convertStringToRM : (String -> Maybe a) -> String -> ResultMaybe String a
convertStringToRM convert str =
    case convert str of
        Just value ->
            Ok (Just value)

        Nothing ->
            if String.isEmpty str then
                Ok Nothing

            else
                Err str


convertStringToRMInt : String -> ResultMaybe String Int
convertStringToRMInt =
    convertStringToRM String.toInt


withNone : Model -> ( Model, Cmd Msg )
withNone model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RestingHRChanged str ->
            withNone { model | restingHR = convertStringToRMInt str }

        AgeChanged str ->
            withNone { model | age = convertStringToRMInt str }

        IntensityChanged str ->
            withNone { model | intensity = convertStringToRMInt str }


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
    div [ class "inputline", style "width" "100%" ]
        [ span
            [ style "width" "20%"
            , style "display" "inline-block"
            ]
            [ text title ]
        , input
            [ style "text-align" "right"
            , style "width" "104px"
            , placeholder placeholder_
            , onInput inputHandler
            , value (toString modelValue)
            , backgroundColor modelValue
            ]
            []
        ]


toStringFromRMInt : ResultMaybe String Int -> String
toStringFromRMInt =
    Result.map (MX.unwrap "" String.fromInt) >> RX.merge


viewIntField :
    String
    -> String
    -> (String -> msg)
    -> ResultMaybe String Int
    -> Html msg
viewIntField title placeholder_ inputHandler modelValue =
    viewInputField title placeholder_ inputHandler modelValue toStringFromRMInt


viewIntensityRange : (String -> msg) -> ResultMaybe String Int -> Html msg
viewIntensityRange inputHandler intensity =
    div [ class "inputline", style "width" "100%" ]
        [ span
            [ style "width" "20%"
            , style "display" "inline-block"
            ]
            [ text "Intensity:" ]
        , input
            [ type_ "range"
            , style "vertical-align" "middle"
            , style "width" "78px"
            , onInput inputHandler
            , HA.min "55"
            , HA.max "95"
            , HA.step "5"
            , value (toStringFromRMInt intensity)
            ]
            []
        , span
            [ style "display" "inline-block"
            , style "text-align" "right"
            , style "font-weight" "normal"
            ]
            [ text (toStringFromRMInt intensity ++ "%") ]
        ]


viewHeartRateRow : String -> Html msg
viewHeartRateRow heartRate =
    div [ class "inputline", style "width" "100%" ]
        [ span
            [ style "width" "20%"
            , style "display" "inline-block"
            ]
            [ text "Heart Rate:" ]
        , span
            [ style "text-align" "right"
            , style "width" "104px"
            ]
            [ text heartRate ]
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
                [ Html.tbody []
                    (tableHeader :: List.map makeTr pairs)
                ]

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
    div [ style "width" "100%" ]
        [ div []
            [ viewIntField "Resting Pulse:" "eg. 65" RestingHRChanged model.restingHR
            , viewIntField "Age:" "eg. 22" AgeChanged model.age
            , viewIntensityRange IntensityChanged model.intensity
            ]
        , div []
            [ viewOutputArea model ]
        ]
