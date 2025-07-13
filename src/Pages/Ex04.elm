module Pages.Ex04 exposing (Model, Msg(..), init, update, view)

import Common.Events exposing (submitOnEnter)
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (on, onInput)
import String exposing (isEmpty)



-- MODEL


type alias Model =
    { noun : String
    , verb : String
    , adjective : String
    , adverb : String
    , message : String
    }


init : Model
init =
    { noun = ""
    , verb = ""
    , adjective = ""
    , adverb = ""
    , message = ""
    }


toStrings : Model -> List String
toStrings m =
    [ m.noun, m.verb, m.adjective, m.adverb ]



-- MSG


type Msg
    = NounChanged String
    | VerbChanged String
    | AdjectiveChanged String
    | AdverbChanged String
    | Submit



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NounChanged str ->
            ( { model | noun = str }, Cmd.none )

        VerbChanged str ->
            ( { model | verb = str }, Cmd.none )

        AdjectiveChanged str ->
            ( { model | adjective = str }, Cmd.none )

        AdverbChanged str ->
            ( { model | adverb = str }, Cmd.none )

        Submit ->
            ( { model | message = makeOutput model }, Cmd.none )


makeOutput : Model -> String
makeOutput model =
    if toStrings model |> List.any isEmpty then
        ""

    else
        "Do you " ++ model.verb ++ " your " ++ model.adjective ++ " " ++ model.noun ++ " " ++ model.adverb ++ "? That's hilarious!"



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ span [] [ text "What is the noun? " ]
            , input
                [ placeholder "eg. dog"
                , value model.noun
                , onInput NounChanged
                , on "keydown" <| submitOnEnter Submit
                ]
                []
            ]
        , div []
            [ span [] [ text "What is the verb? " ]
            , input
                [ placeholder "eg. walk"
                , value model.verb
                , onInput VerbChanged
                , on "keydown" <| submitOnEnter Submit
                ]
                []
            ]
        , div []
            [ span [] [ text "What is the adjective? " ]
            , input
                [ placeholder "eg. blue"
                , value model.adjective
                , onInput AdjectiveChanged
                , on "keydown" <| submitOnEnter Submit
                ]
                []
            ]
        , div []
            [ span [] [ text "What is the adverb? " ]
            , input
                [ placeholder "bg. quickly"
                , value model.adverb
                , onInput AdverbChanged
                , on "keydown" <| submitOnEnter Submit
                ]
                []
            ]
        , div [] [ text model.message ]
        ]
