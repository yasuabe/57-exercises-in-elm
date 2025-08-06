-- ## Ex47: Who’s in Space?
--
-- - Access live data from the Open Notify API (http://api.open-notify.org/astros.json).
-- - Parse the JSON response.
-- - Display:
--     - Total number of people in space.
--     - A table of names and spacecraft.
-- - Do not use pre-downloaded data.


module Pages.Ex47 exposing (Model, Msg(..), astroDataDecoder, fetchAstroData, init, personDecoder, update, view)

import Common.CmdEx exposing (withNone)
import Common.HttpEx exposing (errorToString)
import Common.ResultMaybe exposing (ResultMaybe)
import Html exposing (Html, button, caption, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)



-- MODEL


type alias Person =
    { name : String
    , craft : String
    }


type alias AstroData =
    { people : List Person
    , number : Int
    , message : String
    }


type alias Model =
    { astroData : ResultMaybe Http.Error AstroData
    , loading : Bool
    }


init : Model
init =
    { astroData = Ok Nothing
    , loading = False
    }



-- MSG


type Msg
    = FetchData
    | GotData (Result Http.Error AstroData)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchData ->
            ( { model | loading = True }
            , fetchAstroData
            )

        GotData result ->
            case result of
                Ok astroData ->
                    withNone
                        { model
                            | astroData = Ok <| Just astroData
                            , loading = False
                        }

                Err error ->
                    withNone
                        { model
                            | astroData = Err error
                            , loading = False
                        }



-- HTTP


fetchAstroData : Cmd Msg
fetchAstroData =
    Http.get
        { url = "http://api.open-notify.org/astros.json"
        , expect = Http.expectJson GotData astroDataDecoder
        }



-- DECODERS


astroDataDecoder : Decoder AstroData
astroDataDecoder =
    Decode.map3 AstroData
        (Decode.field "people" (Decode.list personDecoder))
        (Decode.field "number" Decode.int)
        (Decode.field "message" Decode.string)


personDecoder : Decoder Person
personDecoder =
    Decode.map2 Person
        (Decode.field "name" Decode.string)
        (Decode.field "craft" Decode.string)



-- VIEW


viewTable : AstroData -> Html Msg
viewTable data =
    div []
        [ table [ class "contents-table" ]
            [ caption []
                [ text ("Total people in space: " ++ String.fromInt data.number) ]
            , thead []
                [ tr []
                    [ th [ style "width" "70%" ] [ text "Name" ]
                    , th [] [ text "Craft" ]
                    ]
                ]
            , tbody []
                (List.map viewPersonRow data.people)
            ]
        ]


viewPersonRow : Person -> Html Msg
viewPersonRow person =
    tr []
        [ td [] [ text person.name ]
        , td [] [ text person.craft ]
        ]


viewContent : Model -> Html Msg
viewContent { astroData, loading } =
    case ( astroData, loading ) of
        ( _, True ) ->
            text "Loading..."

        ( Ok (Just data), _ ) ->
            viewTable data

        ( Ok Nothing, _ ) ->
            text "Click the button to fetch data"

        ( Err error, _ ) ->
            text ("Error: " ++ errorToString error)


view : Model -> Html Msg
view model =
    div []
        [ div [ class "inputline" ]
            [ button [ onClick FetchData ] [ text "Fetch Astronaut Data" ]
            ]
        , div [] [ viewContent model ]
        ]
