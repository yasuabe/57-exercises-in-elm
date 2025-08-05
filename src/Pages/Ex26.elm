-- # Ex 26: Months to Pay Off a Credit Card
--
-- - Prompt the user for credit card balance, APR (as a percentage), and monthly payment.
-- - Calculate how many months are needed to pay off the balance using the given formula.
-- - Internally convert APR to a daily rate.
-- - Use a function `calculateMonthsUntilPaidOff(balance, apr, payment)` to perform the calculation.
-- - Round up any fractional result to the next whole number.
-- - Do not access input values outside the function.


module Pages.Ex26 exposing (Model, Msg(..), calculateMonthsUntilPaidOff, init, update, view)

import Common.CmdEx exposing (withNone)
import Common.ResultEx as RE
import Common.ResultMaybe exposing (ResultMaybe, convertInputToIntField, map3)
import Common.UI exposing (IntField, intToFieldValue)
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (class, placeholder, style, value)
import Html.Events exposing (onInput)



-- MODEL


type alias Model =
    { balance : IntField
    , apr : IntField
    , payment : IntField
    }


init : Model
init =
    { balance = Ok Nothing
    , apr = Ok Nothing
    , payment = Ok Nothing
    }


calculateMonthsUntilPaidOff : Float -> Float -> Float -> Int
calculateMonthsUntilPaidOff balance apr monthlyPayment =
    let
        f i =
            -(1 / 30)
                * logBase e (1 + (balance / monthlyPayment * (1 - (1 + i) ^ 30)))
                / logBase e (1 + i)
    in
    f (apr / 365) |> ceiling


calcFromModel : Model -> ResultMaybe String Int
calcFromModel model =
    let
        calc balance apr payment =
            calculateMonthsUntilPaidOff
                (toFloat balance)
                (toFloat apr / 100)
                (toFloat payment)
    in
    map3 calc model.balance model.apr model.payment



-- MSG


type Msg
    = BalanceChanged String
    | APRChanged String
    | PaymentChanged String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BalanceChanged value ->
            withNone { model | balance = convertInputToIntField value }

        APRChanged value ->
            withNone { model | apr = convertInputToIntField value }

        PaymentChanged value ->
            withNone { model | payment = convertInputToIntField value }



-- VIEW


backgroundColor : ResultMaybe String a -> Html.Attribute msg
backgroundColor =
    RE.either
        (always <| style "background-color" "inherit")
        (always <| class "error-message")


viewInputField :
    String
    -> String
    -> (String -> msg)
    -> ResultMaybe String Int
    -> Html msg
viewInputField title placeholder_ inputHandler modelValue =
    div [ class "inputline", style "width" "100%" ]
        [ span
            [ style "width" "30%", style "display" "inline-block" ]
            [ text title ]
        , input
            [ style "text-align" "right"
            , style "width" "100px"
            , placeholder placeholder_
            , onInput inputHandler
            , value (intToFieldValue modelValue)
            , backgroundColor modelValue
            ]
            []
        ]


viewOutputArea : Model -> Html msg
viewOutputArea model =
    let
        output months =
            if isNaN (toFloat months) then
                "⚠️ Invalid combination. Please review your values."

            else
                "It will take you "
                    ++ String.fromInt months
                    ++ " months to pay off this card."
    in
    case calcFromModel model of
        Ok (Just months) ->
            div [ class "output" ] [ text <| output months ]

        Ok Nothing ->
            text "Please enter all values."

        Err _ ->
            div [ class "output error-message" ]
                [ text "Please fix the invalid input." ]


view : Model -> Html Msg
view model =
    div [ style "width" "100%" ]
        [ viewInputField "Credit Card Balance:" "eg. 5000" BalanceChanged model.balance
        , viewInputField "APR (as a percent):" "eg. 12" APRChanged model.apr
        , viewInputField "Monthly Payment:" "eg. 100" PaymentChanged model.payment
        , viewOutputArea model
        ]
