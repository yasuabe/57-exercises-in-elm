-- Ex13: Determining Compound Interest
--
-- - Prompt the user for principal principal, interest rate (as a percentage), number of years, and compounding frequency per year.
-- - Convert the interest rate by dividing it by 100.
-- - Use the compound interest formula to compute the final principal.
-- - Round up fractions of a cent to the next penny.
-- - Format the output as money.


module Pages.Ex13 exposing (Model, Msg(..), init, update, view)

import Common.Math exposing (roundToDecimals)
import Common.UI exposing (viewInputField, viewOutputBlock)
import Html exposing (Html, div, input, pre, span, text)
import Html.Attributes exposing (class, readonly)
import List exposing (filterMap, map)
import Maybe exposing (map2)



-- MODEL
-- TODO: duplication


type alias MaybeEither e a =
    Result e (Maybe a)


type alias Model =
    { principal : String
    , rate : String
    , years : String
    , times : String
    , output : Result (List String) (Maybe String)
    }


init : Model
init =
    { principal = ""
    , rate = ""
    , years = ""
    , times = ""
    , output = Ok <| Just "hello"
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
            parseInput "Invalid principal" model.principal

        rate =
            parseInput "Invalid rate" model.rate

        years =
            parseInput "Invalid years" model.years

        times =
            parseInput "Invalid times" model.times

        calcResult =
            case ( ( principal, rate ), ( years, times ) ) of
                ( ( Ok (Just p), Ok (Just r) ), ( Ok (Just y), Ok (Just t) ) ) ->
                    Ok <| Just (p * (1 + (r/100)/t)^(t * y))

                _ ->
                    let
                        x =
                            filterMap
                                (\e ->
                                    case e of
                                        Err m ->
                                            Just m

                                        _ ->
                                            Nothing
                                )
                                [ principal, rate, years, times ]
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
                            ++ (roundToTwoDecimals amount)
                            ++ "."
                    )
                )
                calcResult
    }


roundToTwoDecimals : Float -> String
roundToTwoDecimals x =
    roundToDecimals x 2 |> String.fromFloat



-- TODO: duplication


parseInput : String -> String -> MaybeEither String Float
parseInput errMsg str =
    let
        trimmed =
            String.trim str
    in
    if String.isEmpty trimmed then
        Ok Nothing

    else
        case String.toFloat trimmed of
            Just x ->
                Ok <| Just x

            Nothing ->
                Err errMsg



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInputField
            "What is the principal principal? "
            "e.g. 1500"
            model.principal
            PrincipalChanged
        , viewInputField
            "What is the rate? "
            "e.g. 4.3"
            model.rate
            RateChanged
        , viewInputField
            "What is the number of years? "
            "e.g. 6"
            model.years
            YearsChanged
        , viewInputField
            "What is the number of times the interest is compounded per year? "
            "e.g. 4"
            model.times
            TimesChanged
        , pre [ class "output", readonly True ]
            [ viewOutputBlock model.output "fill all fields"
            ]
        ]
