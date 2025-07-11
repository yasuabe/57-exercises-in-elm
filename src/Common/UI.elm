module Common.UI exposing (..)

import Html exposing (Html, div, input, pre, span, text)
import Html.Attributes exposing (class, placeholder, readonly, value)
import Html.Events exposing (onInput)
import List exposing (map)


viewInputField : String -> String -> String -> (String -> msg) -> Html msg
viewInputField prompt placeholderMsg inputValue onInputHandler =
    div [ class "inputline" ]
        [ span [ class "inputline__prompt" ] [ text prompt ]
        , input
            [ class "inputline__number"
            , placeholder placeholderMsg
            , value inputValue
            , onInput onInputHandler
            ]
            []
        ]


viewOutputBlock : Result (List String) (Maybe String) -> String -> Html msg
viewOutputBlock output whenEmpty =
    case output of
        Ok (Just outputText) ->
            pre [ class "output", readonly True ] [ text outputText ]

        Ok Nothing ->
            pre [ class "output", readonly True ] [ text whenEmpty ]

        Err messages ->
            pre [ class "output error-message", readonly True ] <|
                map (\errorMessage -> div [] [ text errorMessage ]) messages
