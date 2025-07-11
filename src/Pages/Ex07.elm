-- Ex7: Area of a Rectangular Room
--
-- - Prompt the user to enter the length and width of a room in feet.
-- - Calculate the area in square feet.
-- - Convert the area to square meters using a constant conversion factor.
-- - Keep calculations separate from output.
-- - Display both square feet and square meters in the output.


module Pages.Ex07 exposing (Model, Msg(..), init, update, view)

import Common.Math exposing (roundToDecimals)
import Html exposing (Html, div, input, pre, span, text)
import Html.Attributes exposing (class, placeholder, readonly, value)
import Html.Events exposing (onInput)
import List exposing (map)
import Maybe exposing (map2)



-- MODEL


type alias MaybeEither e a =
    Result e (Maybe a)


type alias Model =
    { length : String
    , width : String
    , output : MaybeEither (List String) String
    }


init : Model
init =
    { length = ""
    , width = ""
    , output = Ok Nothing
    }



-- MSG


type Msg
    = LengthChanged String
    | WidthChanged String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LengthChanged str ->
            ( makeOutput { model | length = String.trim str }, Cmd.none )

        WidthChanged str ->
            ( makeOutput { model | width = String.trim str }, Cmd.none )


makeOutput : Model -> Model
makeOutput model =
    let
        lengthInFeet =
            parseInput "Invalid length" model.length

        widthInFeet =
            parseInput "Invalid width" model.width

        areaResult =
            case ( lengthInFeet, widthInFeet ) of
                ( Err e1, Err e2 ) ->
                    Err [ e1, e2 ]

                ( Err e1, _ ) ->
                    Err [ e1 ]

                ( _, Err e2 ) ->
                    Err [ e2 ]

                ( Ok o1, Ok o2 ) ->
                    Ok <| map2 (*) o1 o2
    in
    { model
        | output =
            Result.map
                (Maybe.map
                    (\area ->
                        "You entered dimensions of "
                            ++ model.length
                            ++ " feet by "
                            ++ model.width
                            ++ " feet.\n"
                            ++ "The area is "
                            ++ roundToTwoDecimals area
                            ++ " square feet\n"
                            ++ roundToTwoDecimals (area * 0.09290304)
                            ++ " square meters"
                    )
                )
                areaResult
    }


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


roundToTwoDecimals : Float -> String
roundToTwoDecimals x =
    roundToDecimals x 2 |> String.fromFloat



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewNumberInput
            "What is the length of the room in feet? "
            "e.g. 12.34"
            model.length
            LengthChanged
        , viewNumberInput
            "What is the width of the room in feet? "
            "e.g. 12.34"
            model.width
            WidthChanged
        , pre [ class "output", readonly True ]
            [ viewOutputBlock model ]
        ]


viewNumberInput : String -> String -> String -> (String -> Msg) -> Html Msg -- TODO: use Common.UI.viewInputField
viewNumberInput prompt placeholderMsg inputValue msgType =
    div [ class "inputline" ]
        [ span [ class "inputline__prompt" ] [ text prompt ]
        , input
            [ class "inputline__number"
            , placeholder placeholderMsg
            , value inputValue
            , onInput msgType
            ]
            []
        ]


viewOutputBlock : Model -> Html Msg
viewOutputBlock model =
    case model.output of
        Ok (Just outputText) ->
            pre [ class "output", readonly True ] [ text outputText ]

        Ok Nothing ->
            pre [ class "output", readonly True ] [ text "Please enter both the length and width." ]

        Err messages ->
            pre [ class "output error-message", readonly True ] <|
                map (\errorMessage -> div [] [ text errorMessage ]) messages
