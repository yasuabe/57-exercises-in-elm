module Common.UI exposing (..)

import Common.ResultMaybe exposing (ResultMaybe)
import Html exposing (Attribute, Html, div, input, pre, span, text)
import Html.Attributes exposing (class, placeholder, readonly, value)
import Html.Events exposing (onInput)
import List exposing (map)


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


viewSimpleInput : String -> String -> String -> String -> (String -> msg) -> Html msg
viewSimpleInput style_ prompt placeholderMsg inputValue onInputHandler =
    viewInputFieldWithHandler
        prompt
        placeholderMsg
        style_
        inputValue
        [ onInput onInputHandler ]


viewNumberInput : String -> String -> String -> (String -> msg) -> Html msg
viewNumberInput =
    viewSimpleInput "inputline__number"


viewTextInput : String -> String -> String -> (String -> msg) -> Html msg
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
