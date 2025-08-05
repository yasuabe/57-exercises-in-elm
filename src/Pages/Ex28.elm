-- # Ex28: Adding Numbers
-- ・ Prompt the user to enter five numbers.
-- ・ Use a counted loop to handle repeated prompting.
-- ・ Compute the total of the entered numbers.
-- ・ Display the total at the end.


module Pages.Ex28 exposing (Model, Msg(..), init, update, view)

import Common.CmdEx exposing (withNone)
import Common.ResultMaybe as RM exposing (convertInputToIntField)
import Common.UI exposing (IntField, intToFieldValue, viewSimpleInput)
import Html exposing (Html, div, pre, text)
import Html.Attributes exposing (class, readonly, value)
import List exposing (filterMap, indexedMap, repeat, sum)
import List.Extra exposing (setAt)
import Result.Extra exposing (unpack)
import String



-- MODEL


type alias Model =
    List IntField


init : Model
init =
    repeat 5 (Ok Nothing)



-- MSG


type Msg
    = NumberChanged Int String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NumberChanged n str ->
            withNone <| setAt n (convertInputToIntField str) model


calculateTotal : List IntField -> Int
calculateTotal =
    filterMap RM.toMaybe >> sum



-- VIEW


renderInput : Int -> IntField -> Html Msg
renderInput n value =
    let
        clazz =
            unpack
                (always "inputline__number--invalid")
                (always "inputline__number")
                value
    in
    viewSimpleInput
        clazz
        (NumberChanged n)
        "Enter a number: "
        "e.g. 15"
        (intToFieldValue value)


viewOutputBlock : Int -> Html Msg
viewOutputBlock output =
    pre [ class "output", readonly True ]
        [ div
            []
            [ text <| "The total is " ++ String.fromInt output ++ "." ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [] (indexedMap renderInput model)
        , viewOutputBlock <| calculateTotal model
        ]
