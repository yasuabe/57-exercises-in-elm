--  Ex33: Magic 8 Ball
--
-- - Create a Magic 8 Ball game.
-- - Prompt user for a question.
-- - Randomly reply with one of:
--   - “Yes”
--   - “No”
--   - “Maybe”
--   - “Ask again later”
-- - Use a list (array) and a random number generator to select the response.


module Pages.Ex33 exposing (Model, Msg(..), init, update, view)

import Array exposing (fromList, get)
import Common.ResultMaybe exposing (ResultMaybe)
import Common.UI exposing (viewInputFieldWithHander, viewOutputBlock)
import Html exposing (Html, div, pre, text)
import Html.Attributes exposing (class, readonly)
import Html.Events exposing (on, onInput)
import Json.Decode as Decode
import Random



-- MODEL


responses =
    fromList
        [ "Yes"
        , "No"
        , "Maybe"
        , "Ask again later"
        ]


type alias Model =
    { question : String
    , output : ResultMaybe (List String) String
    }


init : Model
init =
    { question = ""
    , output = Ok <| Nothing
    }



-- MSG


type Msg
    = InputChanged String
    | GotSelectedIndex Int
    | Submit



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
            ( makeOutput model
            , Random.generate GotSelectedIndex randomMessagePicker
            )

        InputChanged str ->
            ( { model | question = str }, Cmd.none )

        GotSelectedIndex index ->
            ( { model | output = Ok <| get index responses }, Cmd.none )


randomMessagePicker : Random.Generator Int
randomMessagePicker =
    Random.int 0 3


makeOutput : Model -> Model
makeOutput model =
    { model | output = Ok <| Just <| String.fromInt <| String.length model.question }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInputFieldWithHander
            "What's your question? "
            "e.g. Will I be rich and famous? "
            "inputline__text"
            model.question
            [ onInput InputChanged
            , on "keydown" (Decode.field "key" Decode.string |> Decode.andThen keyDecoder)
            ]
        , pre [ class "output", readonly True ]
            [ viewOutputBlock model.output "" ]
        ]


keyDecoder : String -> Decode.Decoder Msg
keyDecoder key =
    if key == "Enter" then
        Decode.succeed Submit

    else
        Decode.fail "Not Enter key"
