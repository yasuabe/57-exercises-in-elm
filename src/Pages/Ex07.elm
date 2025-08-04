-- Ex7: Area of a Rectangular Room
--
-- - Prompt the user to enter the length and width of a room in feet.
-- - Calculate the area in square feet.
-- - Convert the area to square meters using a constant conversion factor.
-- - Keep calculations separate from output.
-- - Display both square feet and square meters in the output.


module Pages.Ex07 exposing (Model, Msg(..), init, update, view, makeOutput)

import Common.Events exposing (onBlur, onEnter, withNone)
import Common.Math exposing (roundToDecimals)
import Common.ResultEx as RE
import Common.ResultMaybe as RM exposing (ResultMaybe, convertInputToFloatField)
import Html exposing (Html, div, input, span, text, pre)
import Html.Attributes exposing (class, placeholder, style, value)
import Maybe.Extra as MX
import Result.Extra as RX



-- MODEL


type alias Model =
    { length : ResultMaybe String Float
    , width : ResultMaybe String Float
    }


init : Model
init =
    { length = Ok Nothing
    , width = Ok Nothing
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
            withNone { model | length = convertInputToFloatField str }

        WidthChanged str ->
            withNone { model | width = convertInputToFloatField str }


makeOutput : Model -> ResultMaybe String String
makeOutput model =
    RM.map2
        (\l w ->
            "You entered dimensions of "
                ++ roundToTwoDecimals l
                ++ " feet by "
                ++ roundToTwoDecimals w
                ++ " feet.\n"
                ++ "The area is "
                ++ roundToTwoDecimals (l * w)
                ++ " square feet.\n"
                ++ "That's "
                ++ roundToTwoDecimals (l * w * 0.09290304)
                ++ " square meters."
        )
        model.length
        model.width


roundToTwoDecimals : Float -> String
roundToTwoDecimals =
    roundToDecimals 2 >> String.fromFloat



-- VIEW


toFieldValue : ResultMaybe String Float -> String
toFieldValue =
    Result.map (MX.unwrap "" String.fromFloat) >> RX.merge


viewInputLine : String -> String -> ResultMaybe String Float -> (String -> Msg) -> Html Msg
viewInputLine label placeholder_ value_ onChange =
    let
        backgroundColor =
            RE.either
                (always <| style "background-color" "inherit")
                (always <| class "error-message")
    in
    div [ class "inputline" ]
        [ span [ class "inputline__prompt", style "width" "320px" ] [ text label ]
        , input
            [ class "inputline__number"
            , style "max-width" "75px"
            , placeholder placeholder_
            , value <| toFieldValue value_
            , onEnter onChange
            , onBlur onChange
            , backgroundColor value_
            ]
            []
        ]


viewOutput : Model -> Html Msg
viewOutput model =
    case makeOutput model of
        Ok (Just output) ->
            pre [ class "output" ] [ text output ]

        Ok Nothing ->
            div [ class "output" ] [ text "Enter both fields" ]

        Err errMsg ->
            div [ class "output" ] [ text <| "Invalid input: " ++ errMsg ]


view : Model -> Html Msg
view model =
    div [ style "width" "100%" ]
        [ viewInputLine
            "What is the length of the room in feet? "
            "e.g. 15"
            model.length
            LengthChanged
        , viewInputLine
            "What is the width of the room in feet? "
            "e.g. 20"
            model.width
            WidthChanged
        , viewOutput model
        ]
