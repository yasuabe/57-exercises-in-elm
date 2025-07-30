-- Ex13: Determining Compound Interest
--
-- - Prompt the user for principal principal, interest rate (as a percentage), number of years, and compounding frequency per year.
-- - Convert the interest rate by dividing it by 100.
-- - Use the compound interest formula to compute the final principal.
-- - Round up fractions of a cent to the next penny.
-- - Format the output as money.


module Pages.Ex13 exposing (Model, Msg(..), init, update, view)

import Common.Math exposing (roundToDecimals)
import Common.ResultMaybe exposing (ResultMaybe, collectErrors, map, parseStringToFloat)
import Common.UI exposing (viewNumberInput, viewOutputBlock)
import Html exposing (Html, div, pre)
import Html.Attributes exposing (class, readonly)
import List


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
                    Ok <| Just ( ( p, r ), ( y, t, p * (1 + (r / 100) / t) ^ (t * y) ) )

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
            map
                (\( ( p, r ), ( y, t, amount ) ) ->
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
                        ++ (roundToDecimals 2 amount |> String.fromFloat)
                        ++ "."
                )
                calcResult
    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewNumberInput
            PrincipalChanged
            "What is the principal principal? "
            "e.g. 1500"
            model.principal
        , viewNumberInput
            RateChanged
            "What is the rate? "
            "e.g. 4.3"
            model.rate
        , viewNumberInput
            YearsChanged
            "What is the number of years? "
            "e.g. 6"
            model.years
        , viewNumberInput
            TimesChanged
            "What is the number of times the interest is compounded per year? "
            "e.g. 4"
            model.times
        , pre [ class "output", readonly True ]
            [ viewOutputBlock model.output "fill all fields"
            ]
        ]
