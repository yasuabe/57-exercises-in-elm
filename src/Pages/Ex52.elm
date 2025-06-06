module Pages.Ex52 exposing (Model, Msg(..), init, update, view)


import Html exposing (Html, div, text)



-- MODEL


type alias Model =
    {}


init : Model
init =
    {}



-- MSG


type Msg
    = Submit



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        Submit ->
            ( init
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view _ =
    div []
        [ div [] [ text "not implemented" ]
        ]
