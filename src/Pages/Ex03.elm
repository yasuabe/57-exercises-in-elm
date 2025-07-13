module Pages.Ex03 exposing (Model, Msg(..), init, update, view)

import Common.Events exposing (submitOnEnter)
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (on, onInput)



-- MODEL


type alias Model =
    { quote : String
    , author : String
    , message : String
    }


init : Model
init =
    { quote = ""
    , author = ""
    , message = ""
    }



-- MSG


type Msg
    = QuoteChanged String
    | AuthorChanged String
    | Submit



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthorChanged str ->
            ( { model | author = str }, Cmd.none )

        QuoteChanged str ->
            ( { model | quote = str }, Cmd.none )

        Submit ->
            ( { model | message = makeOutput model }, Cmd.none )


makeOutput : Model -> String
makeOutput model =
    if model.quote /= "" && model.author /= "" then
        model.author ++ " says, " ++ "\"" ++ model.quote ++ "\""

    else
        ""



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ span [] [ text "What is the quote? " ]
            , input
                [ placeholder "Enter a quote"
                , value model.quote
                , onInput QuoteChanged
                , on "keydown" <| submitOnEnter Submit
                ]
                []
            ]
        , div []
            [ span [] [ text "Who said it? " ]
            , input
                [ placeholder "Enter the author"
                , value model.author
                , onInput AuthorChanged
                , on "keydown" <| submitOnEnter Submit
                ]
                []
            ]
        , div [] [ text model.message ]
        ]
