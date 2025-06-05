module Pages.Ex01 exposing (Model, Msg(..), init, update, view)

import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (on, onInput)
import Json.Decode as Decode



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
            ( { model
                | message =
                    if model.input /= "" then
                        "Hello, " ++ model.input ++ ", nice to meet you!"

                    else
                        ""
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ span [] [ text "What is your name? " ]
            , input
                [ placeholder "Enter your name"
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
