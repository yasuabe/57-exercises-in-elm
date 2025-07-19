-- Ex7: Area of a Rectangular Room
--
-- - Prompt the user to enter the length and width of a room in feet.
-- - Calculate the area in square feet.
-- - Convert the area to square meters using a constant conversion factor.
-- - Keep calculations separate from output.
-- - Display both square feet and square meters in the output.


module Pages.Ex07 exposing (Model, Msg(..), init, update, view)

import Common.Math exposing (roundToDecimals)
import Common.ResultMaybe exposing (ResultMaybe, collectErrors, map, parseStringToFloat)
import Common.UI exposing (viewNumberInput, viewOutputBlock)
import Html exposing (Html, div, pre)
import Html.Attributes exposing (class, readonly)
import Maybe exposing (map2)



-- MODEL


type alias Model =
    { length : String
    , width : String
    , output : ResultMaybe (List String) String
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
    let
        newModel =
            case msg of
                LengthChanged str ->
                    makeOutput { model | length = String.trim str }

                WidthChanged str ->
                    makeOutput { model | width = String.trim str }
    in
    ( newModel, Cmd.none )


makeOutput : Model -> Model
makeOutput model =
    let
        lengthInFeet =
            parseStringToFloat "Invalid length" model.length

        widthInFeet =
            parseStringToFloat "Invalid width" model.width

        areaResult =
            case ( lengthInFeet, widthInFeet ) of
                ( Ok o1, Ok o2 ) ->
                    Ok <| map2 (*) o1 o2

                _ ->
                    Err <| collectErrors [ lengthInFeet, widthInFeet ]
    in
    { model
        | output =
            map
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
                areaResult
    }


roundToTwoDecimals : Float -> String
roundToTwoDecimals x =
    roundToDecimals x 2 |> String.fromFloat



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewNumberInput
            LengthChanged
            "What is the length of the room in feet? "
            "e.g. 15"
            model.length
        , viewNumberInput
            WidthChanged
            "What is the width of the room in feet? "
            "e.g. 10"
            model.width
        , pre [ class "output", readonly True ]
            [ viewOutputBlock model ]
        ]


viewOutputBlock : Model -> Html Msg
viewOutputBlock model =
    Common.UI.viewOutputBlock model.output "Please enter both the length and width."
