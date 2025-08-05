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

import Array exposing (Array, fromList, get)
import Common.Events exposing (onEnter)
import Common.ResultMaybe exposing (ResultMaybe)
import Common.UI exposing (viewInputFieldWithHandler, viewOutputBlock)
import Html exposing (Html, div, pre)
import Html.Attributes exposing (class, readonly)
import Random



-- MODEL


responses : Array String
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
    , output = Ok Nothing
    }



-- MSG


type Msg
    = GotSelectedIndex Int
    | Submit String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit str ->
            ( { model | question = str }
            , Random.generate GotSelectedIndex randomMessagePicker
            )

        GotSelectedIndex index ->
            ( { model | output = Ok <| get index responses }, Cmd.none )


randomMessagePicker : Random.Generator Int
randomMessagePicker =
    Random.int 0 3



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInputFieldWithHandler
            "inputline__text"
            [ onEnter Submit ]
            "What's your question? "
            "e.g. Will I be rich and famous? "
            model.question
        , pre
            [ class "output", readonly True ]
            [ viewOutputBlock model.output "" ]
        ]
