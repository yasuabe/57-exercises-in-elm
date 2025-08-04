module Pages.Ex03 exposing (Model, Msg(..), init, makeOutput, update, view)

import Common.CmdEx exposing (withNone)
import Common.MaybeEx exposing (fromMaybe, toMaybe)
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (class, placeholder, style, value)
import Html.Events exposing (onInput)



-- MODEL


type alias Model =
    { quote : Maybe String
    , author : Maybe String
    }


init : Model
init =
    Model Nothing Nothing


makeOutput : Model -> Maybe String
makeOutput model =
    Maybe.map2
        (\author quote -> author ++ " says, " ++ "\"" ++ quote ++ "\"")
        model.author
        model.quote



-- MSG


type Msg
    = QuoteChanged String
    | AuthorChanged String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthorChanged str ->
            withNone { model | author = toMaybe str }

        QuoteChanged str ->
            withNone { model | quote = toMaybe str }



-- VIEW


viewInputLine : String -> String -> Maybe String -> (String -> Msg) -> Html Msg
viewInputLine label placeholder_ value_ onChange =
    div [ class "inputline" ]
        [ span [ class "inputline__prompt", style "width" "24%" ] [ text label ]
        , input
            [ placeholder placeholder_
            , value <| fromMaybe value_
            , onInput onChange
            ]
            []
        ]


viewOutput : Model -> Html Msg
viewOutput model =
    case makeOutput model of
        Just output ->
            div [ class "output" ] [ text output ]

        Nothing ->
            div [ class "output" ] [ text "Enter both fields" ]


view : Model -> Html Msg
view model =
    div [ style "width" "100%" ]
        [ viewInputLine
            "What is the quote? "
            "Enter a quote"
            model.quote
            QuoteChanged
        , viewInputLine
            "Who said it? "
            "Enter the author"
            model.author
            AuthorChanged
        , viewOutput model
        ]
