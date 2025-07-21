-- # Ex28: Adding Numbers
-- ・ Prompt the user to enter five numbers.
-- ・ Use a counted loop to handle repeated prompting.
-- ・ Compute the total of the entered numbers.
-- ・ Display the total at the end.


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
    { inputStrings : Array String
    , total : Int
    }


init : Model
init =
    { inputStrings = repeat 5 "", total = 0 }



-- MSG


type Msg
    = NumberChanged Int String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NumberChanged n str ->
            ( updateTotal { model | inputStrings = set n (trim str) model.inputStrings }
            , Cmd.none
            )


updateTotal : Model -> Model
updateTotal model =
    { model | total = model.inputStrings |> toList |> filterMap toInt |> sum }



-- VIEW


view : Model -> Html Msg
view { inputStrings, total } =
    div [] <|
        (indexedMap renderInput <| toList inputStrings)
            ++ viewOutputBlock total


renderInput : Int -> String -> Html Msg
renderInput n value =
    let
        class =
            if isEmpty value then
                "inputline__number"

            else
                toInt value
                    |> Maybe.map (always "inputline__number")
                    |> withDefault "inputline__number--invalid"
    in
    viewSimpleInput
        class
        (NumberChanged n)
        "Enter a number: "
        "e.g. 15"
        value


viewOutputBlock : Int -> List (Html Msg)
viewOutputBlock output =
    [ pre [ class "output", readonly True ]
        [ div [] [ text <| String.fromInt output ] ]
    ]
