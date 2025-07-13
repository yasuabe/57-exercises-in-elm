module Common.UI exposing (..)

import Html exposing (Attribute, Html, div, input, pre, span, text)
import Html.Attributes exposing (class, placeholder, readonly, value)
import Html.Events exposing (onInput)
import List exposing (map)
import Common.ResultMaybe exposing (ResultMaybe)


viewNumberInput : String -> String -> String -> (String -> msg) -> Html msg
viewNumberInput prompt placeholderMsg inputValue onInputHandler =
    viewInputFieldWithHandler
        prompt
        placeholderMsg
        "inputline__number"
        inputValue
        [ onInput onInputHandler ]


viewInputFieldWithHandler : String -> String -> String -> String -> List (Attribute msg) -> Html msg
viewInputFieldWithHandler prompt placeholderMsg inputClass inputValue handlers =
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
