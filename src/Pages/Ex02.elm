module Pages.Ex02 exposing (Model, Msg(..), init, update, view)

import Common.Events exposing (onEnter)
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput)
import String exposing (fromInt, length)



-- (fromInt, length)
-- MODEL


type alias Model =
    { input : String
    , message : String
    }


init : Model
init =
    { input = ""
    , message = ""
    }



-- MSG


type Msg
    = InputChanged String
    | Submit



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged str ->
            ( { model | input = str }, Cmd.none )

        Submit ->
            ( { model | message = makeOutput model.input }, Cmd.none )


makeOutput : String -> String
makeOutput input =
    if input /= "" then
        input ++ " has " ++ fromInt (length input) ++ " characters."

    else
        ""



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ span [] [ text "What is the input string? " ]
            , input
                [ placeholder "eg. Homer"
                , value model.input
                , onInput InputChanged
                , onEnter Submit
                ]
                []
            ]
        , div [] [ text model.message ]
        ]
