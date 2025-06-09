module Pages.Ex47 exposing (Model, Msg(..), init, update, view, personDecoder, astroDataDecoder, fetchAstroData)

import Html exposing (Html, div, text, table, thead, tbody, caption, tr, th, td, button)
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
    { astroData : Maybe AstroData
    , loading : Bool
    , error : Maybe String
    }


init : Model
init =
    { astroData = Nothing
    , loading = False
    , error = Nothing
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
            ( { model | loading = True, error = Nothing }
            , fetchAstroData
            )
        
        GotData result ->
            case result of
                Ok astroData ->
                    ( { model 
                      | astroData = Just astroData
                      , loading = False
                      }
                    , Cmd.none
                    )
                
                Err _ ->
                    ( { model 
                      | error = Just "Failed to fetch data"
                      , loading = False
                      }
                    , Cmd.none
                    )



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


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick FetchData ] [ text "Fetch Astronaut Data" ]
        , viewContent model
        ]


viewContent : Model -> Html Msg
viewContent model =
    if model.loading then
        div [] [ text "Loading..." ]
    else
        case model.error of
            Just errorMsg ->
                div [] [ text errorMsg ]
            
            Nothing ->
                case model.astroData of
                    Just data ->
                        viewTable data
                    
                    Nothing ->
                        div [] [ text "Click the button to fetch data" ]


viewTable : AstroData -> Html Msg
viewTable data =
    div []
        [ table [ class "contents-table" ]
            [ caption [] [ text ("Total people in space: " ++ String.fromInt data.number) ]
            , thead []
                [ tr []
                    [ th [ style "width" "70%"] [ text "Name" ]
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
