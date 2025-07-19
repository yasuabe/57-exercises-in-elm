module Pages.Ex28 exposing (Model, Msg(..), init, update, view)

import Array exposing (Array, repeat, set, toList)
import Common.UI exposing (viewSimpleInput)
import Html exposing (Html, div, pre, text)
import Html.Attributes exposing (class, readonly)
import List exposing (filterMap, indexedMap, sum)
import Maybe exposing (withDefault)
import String exposing (isEmpty, toInt, trim)



-- MODEL


type alias Model =
    { inputNumbers : Array String
    , output : Int
    }


init : Model
init =
    { inputNumbers = repeat 5 "", output = 0 }



-- MSG


type Msg
    = NumberChanged Int String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NumberChanged n str ->
            ( makeOutput { model | inputNumbers = set n (trim str) model.inputNumbers }
            , Cmd.none
            )


makeOutput : Model -> Model
makeOutput model =
    { model | output = toList model.inputNumbers |> filterMap toInt |> sum }



-- VIEW


view : Model -> Html Msg
view { inputNumbers, output } =
    div [] <| (indexedMap renderNumberInput <| toList inputNumbers) ++ [ viewOutputBlock output ]


renderNumberInput : Int -> String -> Html Msg
renderNumberInput n value =
    let
        class =
            if isEmpty value then
                "inputline__number"

            else
                toInt value |> Maybe.map (always "inputline__number") |> withDefault "inputline__number--invalid"
    in
    viewSimpleInput
        class
        (NumberChanged n)
        "Enter a number: "
        "e.g. 15"
        value


viewOutputBlock : Int -> Html Msg
viewOutputBlock output =
    pre [ class "output", readonly True ]
        [ div [] [ text <| String.fromInt output ] ]
