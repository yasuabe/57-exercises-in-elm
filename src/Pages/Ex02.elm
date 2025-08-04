module Pages.Ex02 exposing (Model, Msg(..), init, makeOutput, update, view)

import Common.Events exposing (onEnter)
import Common.MaybeEx as ME
import Html exposing (Html, div, input, kbd, span, text)
import Html.Attributes exposing (class, placeholder, value)
import String exposing (fromInt, length)



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


makeOutput : Model -> Maybe String
makeOutput =
    Maybe.map (\s -> s ++ " has " ++ fromInt (length s) ++ " characters.")



-- MSG


type Msg
    = Submit String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        Submit str ->
            ( fromString str, Cmd.none )



-- VIEW


viewOutput : Model -> Html Msg
viewOutput model =
    case makeOutput model of
        Just countResult ->
            div [ class "output" ] [ text countResult ]

        Nothing ->
            div [ class "output" ]
                [ text "Enter some string and press "
                , kbd [] [ text "Enter" ]
                ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "inputline" ]
            [ span [ class "inputline__prompt" ] [ text "What is the input string? " ]
            , input
                [ placeholder "eg. Homer"
                , value <| toString model
                , onEnter Submit
                ]
                []
            ]
        , viewOutput model
        ]
