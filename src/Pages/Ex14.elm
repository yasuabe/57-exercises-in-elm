module Pages.Ex14 exposing (Model, Msg(..), init, update, view)

import Common.ResultMaybe as ResultMaybe exposing (ResultMaybe, collectErrors2, parseString, parseStringToFloat)
import Common.UI exposing (viewNumberInput, viewOutputBlock)
import Common.USStates exposing (findANSICode, usStates)
import Dict exposing (Dict)
import Html exposing (Html, datalist, div, input, option, pre, span, text)
import Html.Attributes exposing (class, id, list, placeholder, readonly, value)
import Html.Events exposing (onInput)
import Maybe exposing (withDefault)
import Round as R



-- MODEL


taxRates : Dict String Float
taxRates =
    Dict.fromList
        [ ( "WI", 0.055 )
        ]


type alias Model =
    { orderAmount : String
    , state : String
    , output : ResultMaybe (List String) String
    }


init : Model
init =
    { orderAmount = ""
    , state = ""
    , output = Ok Nothing
    }



-- MSG


type Msg
    = AmountChanged String
    | StateChanged String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                AmountChanged str ->
                    makeOutput { model | orderAmount = String.trim str }

                StateChanged str ->
                    makeOutput { model | state = String.trim str }
    in
    ( newModel, Cmd.none )


makeOutput : Model -> Model
makeOutput model =
    let
        orderAmountResult =
            parseStringToFloat "Invalid order amount" model.orderAmount

        stateResult =
            parseString findANSICode "Invalid state input" model.state

        taxCaculationResult =
            case ( orderAmountResult, stateResult ) of
                ( Ok (Just amount), Ok (Just stateCode) ) ->
                    Dict.get stateCode taxRates
                        |> Maybe.map (\rate -> ( amount, Just <| amount * rate ))
                        |> withDefault ( amount, Nothing )
                        |> Just
                        |> Ok

                ( Ok _, Ok _ ) ->
                    Ok Nothing

                _ ->
                    Err <|
                        collectErrors2 orderAmountResult stateResult
    in
    { model
        | output =
            ResultMaybe.map
                (\( amount, maybeTax ) ->
                    case maybeTax of
                        Just t ->
                            String.join "\n"
                                [ "The subtotal is $" ++ R.ceiling 2 amount
                                , "The tax is $" ++ R.ceiling 2 t
                                , "The total is $" ++ R.ceiling 2 (amount + t)
                                ]

                        Nothing ->
                            "The total is " ++ R.ceiling 2 amount
                )
                taxCaculationResult
    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewNumberInput
            AmountChanged
            "What is the order amount? "
            "e.g. 10"
            model.orderAmount
        , viewStateInput
            StateChanged
            "What is the state? "
            "e.g. WI"
            model.state
        , pre [ class "output", readonly True ]
            [ viewOutputBlock model.output "Enter order amount and state" ]
        ]


viewStateInput : (String -> msg) -> String -> String -> String -> Html msg
viewStateInput onInputHandler prompt placeHolder inputValue =
    div [ class "inputline" ]
        [ span [ class "inputline__prompt" ] [ text prompt ]
        , input
            [ class "inputline__text"
            , placeholder placeHolder
            , value inputValue
            , onInput onInputHandler
            , list "states"
            ]
            []
        , datalist [ id "states" ]
            (List.map
                (\( code, _ ) ->
                    option [ value code ] [ text code ]
                )
                usStates
            )
        ]
