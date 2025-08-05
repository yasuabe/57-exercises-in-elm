-- Ex13: Determining Compound Interest
--
-- - Prompt the user for principal, interest rate (as a percentage), number of years, and compounding frequency per year.
-- - Convert the interest rate by dividing it by 100.
-- - Use the compound interest formula to compute the final amount.
-- - Round up fractions of a cent to the next penny.
-- - Format the output as money.


module Pages.Ex13 exposing (Model, Msg(..), init, update, view, makeOutput)

import Common.CmdEx exposing (withNone)
import Common.Events exposing (onBlur, onEnter)
import Common.Math exposing (roundToDecimals)
import Common.ResultEx as RE
import Common.ResultMaybe exposing (ResultMaybe, convertInputToFloatField, map4)
import Common.UI exposing (floatToFieldValue, viewOutputBlock)
import Html exposing (Html, div, input, pre, span, text)
import Html.Attributes exposing (class, placeholder, readonly, value)



-- MODEL

-- TODO: duplicate with UI.elm

type alias FloatField =
    ResultMaybe String Float


type alias Model =
    { principal : FloatField
    , rate : FloatField
    , years : FloatField
    , times : FloatField
    }


init : Model
init =
    { principal = Ok Nothing
    , rate = Ok Nothing
    , years = Ok Nothing
    , times = Ok Nothing
    }


calcAmount : Float -> Float -> Float -> Float -> Float
calcAmount principal rate years times =
    principal * (1 + (rate / 100) / times) ^ (times * years)



-- MSG


type Msg
    = PrincipalChanged String
    | RateChanged String
    | YearsChanged String
    | TimesChanged String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PrincipalChanged str ->
            withNone { model | principal = convertInputToFloatField str }

        RateChanged str ->
            withNone { model | rate = convertInputToFloatField str }

        YearsChanged str ->
            withNone { model | years = convertInputToFloatField str }

        TimesChanged str ->
            withNone { model | times = convertInputToFloatField str }


makeOutput : Model -> ResultMaybe String String
makeOutput model =
    map4
        (\p r y t ->
            "$"
                ++ String.fromFloat p
                ++ " invested at "
                ++ String.fromFloat r
                ++ "% for "
                ++ String.fromFloat y
                ++ " years\n"
                ++ "compounded "
                ++ String.fromFloat t
                ++ " times per year is $"
                ++ (roundToDecimals 2 (calcAmount p r y t) |> String.fromFloat)
                ++ "."
        )
        model.principal
        model.rate
        model.years
        model.times



-- VIEW


viewNumberInput : String -> String -> FloatField -> (String -> msg) -> Html msg
viewNumberInput prompt placeholder_ inputValue handler =
    let
        class_ =
            RE.either
                (always <| "inputline__number ex13__inputline-number")
                (always <| "inputline__number ex13__inputline-number error-message")
    in
    div [ class "inputline" ]
        [ span [ class "inputline__prompt ex13__inputline-label" ] [ text prompt ]
        , input
            [ class <| class_ inputValue
            , placeholder placeholder_
            , value <| floatToFieldValue inputValue
            , onEnter handler
            , onBlur handler
            ]
            []
        ]


viewOutputBlock : ResultMaybe String String -> Html msg
viewOutputBlock result =
    case result of
        Ok (Just output) ->
            pre [ class "output", readonly True ] [ text output ]

        Ok Nothing ->
            pre [ class "output", readonly True ] [ text "Please fill all fields." ]

        Err _ ->
            pre [ class "output", readonly True ] [ text "Please fix the inputs" ]


view : Model -> Html Msg
view model =
    div []
        [ viewNumberInput
            "What is the principal? "
            "e.g. 1500"
            model.principal
            PrincipalChanged
        , viewNumberInput
            "What is the interest rate? "
            "e.g. 4.3"
            model.rate
            RateChanged
        , viewNumberInput
            "What is the number of years? "
            "e.g. 6"
            model.years
            YearsChanged
        , viewNumberInput
            "What is the number of times the interest is compounded per year? "
            "e.g. 4"
            model.times
            TimesChanged
        , div [ class "output" ]
            [ viewOutputBlock (makeOutput model) ]
        ]
