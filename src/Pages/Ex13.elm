-- Ex13: Determining Compound Interest
--
-- - Prompt the user for principal principal, interest rate (as a percentage), number of years, and compounding frequency per year.
-- - Convert the interest rate by dividing it by 100.
-- - Use the compound interest formula to compute the final principal.
-- - Round up fractions of a cent to the next penny.
-- - Format the output as money.


module Pages.Ex13 exposing (Model, Msg(..), init, update, view)

import Common.Math exposing (roundToDecimals)
import Common.ResultMaybe exposing (ResultMaybe, collectErrors, parseStringToFloat)
import Common.UI exposing (viewNumberInput, viewOutputBlock)
import Html exposing (Html, div, pre)
import Html.Attributes exposing (class, readonly)
import List
import Maybe



-- MODEL


type alias Model =
    { principal : String
    , rate : String
    , years : String
    , times : String
    , output : ResultMaybe (List String) String
    }


init : Model
init =
    { principal = ""
    , rate = ""
    , years = ""
    , times = ""
    , output = Ok <| Nothing
    }



-- MSG


type Msg
    = PrincipalChanged String
    | RateChanged String
    | YearsChanged String
    | TimesChanged String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        PrincipalChanged str ->
            makeOutput { model | principal = String.trim str }

        RateChanged str ->
            makeOutput { model | rate = String.trim str }

        YearsChanged str ->
            makeOutput { model | years = String.trim str }

        TimesChanged str ->
            makeOutput { model | times = String.trim str }
    , Cmd.none
    )


makeOutput : Model -> Model
makeOutput model =
    let
        principal =
            parseStringToFloat "Invalid principal" model.principal

        rate =
            parseStringToFloat "Invalid rate" model.rate

        years =
            parseStringToFloat "Invalid years" model.years

        times =
            parseStringToFloat "Invalid times" model.times

        calcResult =
            case ( ( principal, rate ), ( years, times ) ) of
                ( ( Ok (Just p), Ok (Just r) ), ( Ok (Just y), Ok (Just t) ) ) ->
                    Ok <| Just (p * (1 + (r / 100) / t) ^ (t * y))

                _ ->
                    let
                        x =
                            collectErrors [ principal, rate, years, times ]
                    in
                    if List.isEmpty x then
                        Ok Nothing

                    else
                        Err x
    in
    { model
        | output =
            Result.map
                (Maybe.map
                    (\amount ->
                        "$"
                            ++ "1500"
                            ++ " invested at "
                            ++ "4.3"
                            ++ "% for "
                            ++ "6"
                            ++ " years\n"
                            ++ "compounded "
                            ++ "4"
                            ++ " times per year is $"
                            ++ roundToTwoDecimals amount
                            ++ "."
                    )
                )
                calcResult
    }


roundToTwoDecimals : Float -> String
roundToTwoDecimals x =
    roundToDecimals x 2 |> String.fromFloat



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewNumberInput
            "What is the principal principal? "
            "e.g. 1500"
            model.principal
            PrincipalChanged
        , viewNumberInput
            "What is the rate? "
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
        , pre [ class "output", readonly True ]
            [ viewOutputBlock model.output "fill all fields"
            ]
        ]
