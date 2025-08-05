module Pages.Ex24 exposing (Model, Msg(..), init, update, view, makeOutput)

import Common.CmdEx exposing (withNone)
import Common.Events exposing (onEnter)
import Common.Function exposing (choose, on)
import Common.MaybeEx exposing (toMaybe)
import Common.ResultMaybe exposing (ResultMaybe)
import Common.StringEx exposing (sort)
import Common.UI
import Html exposing (Html, div, input, pre, span, text)
import Html.Attributes exposing (class, placeholder, readonly, value)
import Html.Events exposing (onInput)
import String exposing (length)
import String.Extra exposing (quote)



-- MODEL


type alias Model =
    { first : Maybe String
    , second : Maybe String
    }


init : Model
init =
    { first = Nothing
    , second = Nothing
    }



-- MSG


type Msg
    = FirstChanged String
    | SecondChanged String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                FirstChanged str ->
                    { model | first = toMaybe str }

                SecondChanged str ->
                    { model | second = toMaybe str }
    in
    withNone newModel


isAnagram : String -> String -> Bool
isAnagram =
    on (==) (String.toLower >> sort)


makeOutput : Model -> ResultMaybe (List String) String
makeOutput model =
    let
        checkAnagram first second =
            if on (/=) length first second then
                Err [ "The two strings must have the same length to be anagrams." ]

            else
                quote first
                    ++ " and "
                    ++ quote second
                    ++ " are "
                    ++ choose (isAnagram first second) "" "not "
                    ++ "anagrams."
                    |> (Just >> Ok)
    in
    Maybe.map2 checkAnagram model.first model.second
        |> Maybe.withDefault (Ok Nothing)



-- VIEW


viewTextField : String -> String -> Maybe String -> (String -> msg) -> Html msg
viewTextField prompt placeholderMsg inputValue onInputHandler =
    div [ class "inputline" ]
        [ span [ class "inputline__prompt ex24__label" ] [ text prompt ]
        , input
            [ class "inputline__text ex24__input"
            , placeholder placeholderMsg
            , value <| Maybe.withDefault "" inputValue
            , onEnter onInputHandler
            , onInput onInputHandler
            ]
            []
        ]


viewDescriptionBlock : Html Msg
viewDescriptionBlock =
    div [ class "description ex24__description" ]
        [ text "Enter two strings and I'll tell you if they are anagrams:" ]


viewOutputBlock : Model -> Html Msg
viewOutputBlock model =
    Common.UI.viewOutputBlock
        (makeOutput model)
        "Please enter both the length and width."


view : Model -> Html Msg
view model =
    div []
        [ viewDescriptionBlock
        , div
            [ class "ex24__inputs" ]
            [ viewTextField
                "What is the first? "
                "e.g. note"
                model.first
                FirstChanged
            , viewTextField
                "What is the second? "
                "e.g. tone"
                model.second
                SecondChanged
            ]
        , pre
            [ class "output", readonly True ]
            [ viewOutputBlock model ]
        ]
