module Pages.Ex04 exposing (Model, Msg(..), init, update, view, makeOutput)

import Common.CmdEx exposing (withNone)
import Common.MaybeEx exposing (fromMaybe, toMaybe)
import Html exposing (Html, div, input, pre, span, text)
import Html.Attributes exposing (class, placeholder, style, value)
import Html.Events exposing (onInput)



-- MODEL


type alias Model =
    { noun : Maybe String
    , verb : Maybe String
    , adjective : Maybe String
    , adverb : Maybe String
    }


init : Model
init =
    { noun = Nothing
    , verb = Nothing
    , adjective = Nothing
    , adverb = Nothing
    }



-- MSG


type Msg
    = NounChanged String
    | VerbChanged String
    | AdjectiveChanged String
    | AdverbChanged String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NounChanged str ->
            withNone { model | noun = toMaybe str }

        VerbChanged str ->
            withNone { model | verb = toMaybe str }

        AdjectiveChanged str ->
            withNone { model | adjective = toMaybe str }

        AdverbChanged str ->
            withNone { model | adverb = toMaybe str }


makeOutput : Model -> Maybe String
makeOutput model =
    Maybe.map4
        (\n v a adv ->
            "Do you " ++ v ++ " your " ++ a ++ " " ++ n ++ " " ++ adv ++ "?\nThat's hilarious!"
        )
        model.noun
        model.verb
        model.adjective
        model.adverb


viewInputLine : String -> String -> (String -> Msg) -> Maybe String -> Html Msg
viewInputLine label placeholder_ onChange value_ =
    div [ class "inputline" ]
        [ span [ class "inputline__prompt", style "width" "32%" ] [ text label ]
        , input
            [ placeholder placeholder_
            , value (fromMaybe value_)
            , onInput onChange
            ]
            []
        ]


viewOutputBlock : Maybe String -> Html Msg
viewOutputBlock maybeOutput =
    case maybeOutput of
        Just output ->
            pre [ class "output" ] [ text output ]

        Nothing ->
            div [ class "output" ] [ text "Enter all fields " ]


view : Model -> Html Msg
view model =
    div [ style "width" "100%" ]
        [ viewInputLine "What is the noun? " "eg. dog" NounChanged model.noun
        , viewInputLine "What is the verb? " "eg. walk" VerbChanged model.verb
        , viewInputLine "What is the adjective? " "eg. blue" AdjectiveChanged model.adjective
        , viewInputLine "What is the adverb? " "eg. quickly" AdverbChanged model.adverb
        , viewOutputBlock (makeOutput model)
        ]
