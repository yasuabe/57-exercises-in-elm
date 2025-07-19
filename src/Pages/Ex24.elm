module Pages.Ex24 exposing (Model, Msg(..), init, update, view)

import Common.Function exposing (on)
import Common.ResultMaybe exposing (ResultMaybe)
import Common.StringEx as StringEx exposing (quote, sort)
import Common.UI exposing (viewTextInput)
import Html exposing (Html, div, pre, text)
import Html.Attributes exposing (class, readonly)
import String exposing (isEmpty, length)



-- MODEL


type alias Model =
    { first : String
    , second : String
    , output : ResultMaybe (List String) String
    }


init : Model
init =
    { first = ""
    , second = ""
    , output = Ok Nothing
    }



-- MSG


type Msg
    = FirstChanged String
    | SecondChanged String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                FirstChanged str ->
                    makeOutput { model | first = String.trim str }

                SecondChanged str ->
                    makeOutput { model | second = String.trim str }
    in
    ( newModel, Cmd.none )


isAnagram : Model  -> Bool
isAnagram { first, second } =
    let
        normalize =
            String.toLower >> sort
    in
    on (==) normalize first second


makeOutput : Model -> Model
makeOutput model =
    let
        quote =
            StringEx.quote '"'

        output =
            if on (||) isEmpty model.first model.second then
                Ok Nothing

            else if on (/=) length model.first model.second then
                Err [ "The two strings must have the same length to be anagrams." ]

            else
                let
                    maybeNot =
                        if isAnagram model then
                            ""

                        else
                            "not "
                in
                Ok <| Just <| quote model.first ++ " and " ++ quote model.second ++ " are " ++ maybeNot ++ "anagrams."
    in
    { model | output = output }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewDescriptionBlock
        , viewTextInput
            "What is the first? "
            "e.g. note"
            model.first
            FirstChanged
        , viewTextInput
            "What is the second? "
            "e.g. tone"
            model.second
            SecondChanged
        , pre [ class "output", readonly True ]
            [ viewOutputBlock model ]
        ]


viewDescriptionBlock : Html Msg
viewDescriptionBlock =
    div [ class "description" ] [ text "Enter two strings and I'll tell you if they are anagrams:" ]


viewOutputBlock : Model -> Html Msg
viewOutputBlock model =
    Common.UI.viewOutputBlock model.output "Please enter both the length and width."
