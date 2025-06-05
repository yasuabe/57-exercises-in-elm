module Pages.Ex02 exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (on, onInput)
import Json.Decode as Decode
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
                [ placeholder "Enter a string"
                , value model.input
                , onInput InputChanged
                , on "keydown" (Decode.field "key" Decode.string |> Decode.andThen keyDecoder)
                ]
                []
            ]
        , div [] [ text model.message ]
        ]


keyDecoder : String -> Decode.Decoder Msg
keyDecoder key =
    if key == "Enter" then
        Decode.succeed Submit

    else
        Decode.fail "Not Enter key"
