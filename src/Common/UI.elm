module Common.UI exposing (..)

import Common.ResultMaybe exposing (ResultMaybe)
import Html exposing (Attribute, Html, div, input, pre, span, text)
import Html.Attributes exposing (class, placeholder, readonly, value)
import Html.Events exposing (onInput)
import List exposing (map)
import Maybe.Extra as MX
import Result.Extra as RX


viewInputFieldWithHandler : String -> List (Attribute msg) -> String -> String -> String -> Html msg
viewInputFieldWithHandler inputClass handlers prompt placeholderMsg inputValue =
    div [ class "inputline" ]
        [ span [ class "inputline__prompt" ] [ text prompt ]
        , input
            ([ class inputClass
             , placeholder placeholderMsg
             , value inputValue
             ]
                ++ handlers
            )
            []
        ]


viewSimpleInput : String -> (String -> msg) -> String -> String -> String -> Html msg
viewSimpleInput inputClass onInputHandler =
    viewInputFieldWithHandler
        inputClass
        [ onInput onInputHandler ]


viewNumberInput : (String -> msg) -> String -> String -> String -> Html msg
viewNumberInput =
    viewSimpleInput "inputline__number"


viewTextInput : (String -> msg) -> String -> String -> String -> Html msg
viewTextInput =
    viewSimpleInput "inputline__text"


viewOutputBlock : ResultMaybe (List String) String -> String -> Html msg
viewOutputBlock output whenEmpty =
    case output of
        Ok (Just outputText) ->
            pre [ class "output", readonly True ] [ text outputText ]

        Ok Nothing ->
            pre [ class "output", readonly True ] [ text whenEmpty ]

        Err messages ->
            pre [ class "output error-message", readonly True ] <|
                map (\errorMessage -> div [] [ text errorMessage ]) messages



-- Input Field Converters


type alias FloatField =
    ResultMaybe String Float


type alias TextField =
    ResultMaybe String String


toFieldValue : (a -> String) -> ResultMaybe String a -> String
toFieldValue convert =
    Result.map (MX.unwrap "" convert) >> RX.merge


intToFieldValue : ResultMaybe String Int -> String
intToFieldValue =
    toFieldValue String.fromInt


floatToFieldValue : ResultMaybe String Float -> String
floatToFieldValue =
    toFieldValue String.fromFloat


mergeToStringValue : ResultMaybe String String -> String
mergeToStringValue =
    Result.map (Maybe.withDefault "") >> RX.merge
