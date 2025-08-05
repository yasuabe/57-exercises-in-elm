-- Ex14: Tax Calculator
--
-- - Prompt the user for the order amount and the state.
-- - If the state is "WI", calculate 5.5% tax and display subtotal, tax, and total.
-- - For other states, display only the total.
-- - Use only a simple if statement (no else clause).
-- - Round all money up to the nearest cent.
-- - Use a single output statement at the end.


module Pages.Ex14 exposing (Model, Msg(..), init, makeOutput, update, view)

import Common.CmdEx exposing (withNone)
import Common.Events exposing (onBlur, onEnter)
import Common.ResultEx as RE
import Common.ResultMaybe as RM exposing (ResultMaybe, convertInputToFloatField)
import Common.UI exposing (FloatField, floatToFieldValue, mergeToStringValue, viewNumberInput)
import Common.USStates exposing (findANSICode, usStates)
import Dict exposing (Dict)
import Html exposing (Html, datalist, div, input, option, pre, span, text)
import Html.Attributes exposing (class, id, list, placeholder, value)
import Html.Events exposing (onInput)
import Round as R
import Tuple exposing (pair)



-- MODEL


type alias StateField =
    ResultMaybe String String


taxRates : Dict String Float
taxRates =
    Dict.fromList [ ( "WI", 0.055 ) ]


type alias Model =
    { orderAmount : FloatField
    , state : StateField
    }


init : Model
init =
    { orderAmount = Ok Nothing
    , state = Ok Nothing
    }


calculateTax : Float -> String -> ( Float, Maybe Float )
calculateTax amount stateCode =
    Dict.get stateCode taxRates
        |> Maybe.map ((*) amount)
        |> pair amount



-- MSG


type Msg
    = AmountChanged String
    | StateChanged String



-- UPDATE


convertToStateField : String -> ResultMaybe String String
convertToStateField str =
    if String.isEmpty <| String.trim str then
        Ok Nothing

    else
        case findANSICode str of
            Just code ->
                Ok (Just code)

            Nothing ->
                Err str


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AmountChanged str ->
            withNone { model | orderAmount = convertInputToFloatField str }

        StateChanged str ->
            withNone { model | state = convertToStateField str }


makeOutput : Model -> ResultMaybe String String
makeOutput model =
    RM.map2
        (\amount state ->
            let
                ( subtotal, maybeTax ) =
                    calculateTax amount state
            in
            case maybeTax of
                Just tax ->
                    String.join "\n"
                        [ "The subtotal is $" ++ R.ceiling 2 subtotal
                        , "The tax is $" ++ R.ceiling 2 tax
                        , "The total is $" ++ R.ceiling 2 (subtotal + tax)
                        ]

                Nothing ->
                    "The total is $" ++ R.ceiling 2 subtotal
        )
        model.orderAmount
        model.state



-- VIEW


viewNumberInput : String -> String -> FloatField -> (String -> msg) -> Html msg
viewNumberInput prompt placeholder_ inputValue handler =
    let
        class_ =
            RE.either
                (always <| "inputline__number ex14__inputline-input")
                (always <| "inputline__number ex14__inputline-input error-message")
                inputValue
    in
    div [ class "inputline" ]
        [ span
            [ class "inputline__prompt ex14__inputline-label" ]
            [ text prompt ]
        , input
            [ class <| class_
            , placeholder placeholder_
            , value <| floatToFieldValue inputValue
            , onEnter handler
            , onBlur handler
            ]
            []
        ]


viewStateInput : (String -> msg) -> String -> String -> StateField -> Html msg
viewStateInput onInputHandler prompt placeholder_ inputValue =
    let
        class_ =
            RE.either
                (always <| "inputline__text ex14__inputline-input")
                (always <| "inputline__text ex14__inputline-input error-message")
                inputValue
    in
    div [ class "inputline" ]
        [ span
            [ class "inputline__prompt ex14__inputline-label" ]
            [ text prompt ]
        , input
            [ class class_
            , placeholder placeholder_
            , value <| mergeToStringValue inputValue
            , onInput onInputHandler
            , list "states"
            ]
            []
        , datalist [ id "states" ]
            (List.map
                (\( code, _ ) -> option [ value code ] [ text code ])
                usStates
            )
        ]


viewOutput : ResultMaybe String String -> Html msg
viewOutput result =
    case result of
        Ok (Just output) ->
            div [ class "output" ]
                [ pre [] [ text output ] ]

        Ok Nothing ->
            div [ class "output" ] [ text "No valid output available." ]

        Err _ ->
            div [ class "output" ] [ text "Please fix the input errors." ]


view : Model -> Html Msg
view model =
    div []
        [ viewNumberInput
            "What is the order amount? "
            "e.g. 10"
            model.orderAmount
            AmountChanged
        , viewStateInput
            StateChanged
            "What is the state? "
            "e.g. WI"
            model.state
        , viewOutput (makeOutput model)
        ]
