module Pages.Ex01 exposing (Model, Msg(..), init, update, view, makeGreeting)

import Common.Events exposing (onEnter)
import Common.MaybeEx as ME
import Html exposing (Html, div, input, kbd, span, text)
import Html.Attributes exposing (class, placeholder, value)



-- MODEL


type alias Model =
    Maybe String


init : Model
init =
    Nothing


fromString : String -> Model
fromString =
    ME.fromFilter (String.isEmpty >> not)


toString : Model -> String
toString =
    Maybe.withDefault ""


makeGreeting : Model -> Maybe String
makeGreeting =
    Maybe.map (\input -> "Hello, " ++ input ++ ", nice to meet you!")


type Msg
    = Submit String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        Submit str ->
            ( fromString str, Cmd.none )


viewOutput : Model -> Html Msg
viewOutput model =
    case makeGreeting model of
        Just greeting ->
            div [ class "output" ] [ text greeting ]

        Nothing ->
            div [ class "output" ]
                [ text "Enter your name and press "
                , kbd [] [ text "Enter" ]
                ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "inputline" ]
            [ span [ class "inputline__prompt" ] [ text "What is your name? " ]
            , input
                [ class "inputline__text"
                , placeholder "e.g. Brian"
                , value <| toString model
                , onEnter Submit
                ]
                []
            ]
        , viewOutput model
        ]
