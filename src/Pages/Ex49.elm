-- # Ex49: Flickr Photo Search
--
-- - Take in a search string via GUI.
-- - Fetch Flickr’s public photo feed matching the search.
-- - Display resulting photos visually.


module Pages.Ex49 exposing (Model, Msg(..), decode, init, update, view)

import Common.CmdEx exposing (withNone)
import Common.Events exposing (onEnter)
import Common.HttpEx as HE
import Common.UI exposing (displayNone)
import Html exposing (Html, div, img, input, span, text)
import Html.Attributes exposing (alt, class, placeholder, src, type_)
import Http
import Json.Decode as D exposing (errorToString)
import Result exposing (Result)
import String as S exposing (dropLeft, dropRight)



-- MODEL


type alias Model =
    { feedResult : Result String (List String)
    , status : Maybe String
    }


init : Model
init =
    { feedResult =
        Ok
            [ "https://live.staticflickr.com/65535/54707576688_8a8a918217_m.jpg"
            , "https://live.staticflickr.com/65535/54707576728_e21872c1c9_m.jpg"
            , "https://live.staticflickr.com/65535/54706545637_efbfb60dba_m.jpg"
            , "https://live.staticflickr.com/65535/54707576693_18b8147ba1_m.jpg"
            , "https://live.staticflickr.com/65535/54707326984_50190acedc_m.jpg"
            , "https://live.staticflickr.com/65535/54707103091_bd15e6558a_m.jpg"
            , "https://live.staticflickr.com/65535/54707328624_b5aacd7f2f_m.jpg"
            , "https://live.staticflickr.com/65535/54707250534_7331dce650_m.jpg"
            , "https://live.staticflickr.com/65535/54706809341_55091b7759_m.jpg"
            , "https://live.staticflickr.com/65535/54706653135_7e7ff17c4f_m.jpg"
            , "https://live.staticflickr.com/65535/54706033088_b007f10dc7_m.jpg"
            , "https://live.staticflickr.com/65535/54704786682_d621c7efb0_m.jpg"
            , "https://live.staticflickr.com/65535/54705535839_734b1fbfa7_m.jpg"
            , "https://live.staticflickr.com/65535/54705257821_6c62da8504_m.jpg"
            , "https://live.staticflickr.com/65535/54705478348_fbcef96423_m.jpg"
            , "https://live.staticflickr.com/65535/54705462263_e41b2c1c7c_m.jpg"
            , "https://live.staticflickr.com/65535/54705243868_627d3d7847_m.jpg"
            , "https://live.staticflickr.com/65535/54705368885_aedfb0d135_m.jpg"
            , "https://live.staticflickr.com/65535/54705034676_5a2c65907e_m.jpg"
            , "https://live.staticflickr.com/65535/54705369000_5e7fcc6159_m.jpg"
            ]
    , status = Nothing
    }


decode : String -> Result String (List String)
decode =
    let
        stripJsonp =
            dropLeft (S.length "jsonFlickrFeed(") >> dropRight (S.length ")")

        decoder =
            D.field "items" <|
                D.list <|
                    D.at [ "media", "m" ] D.string
    in
    stripJsonp >> D.decodeString decoder >> Result.mapError errorToString



-- MSG


type Msg
    = FetchFeed String
    | GetText (Result Http.Error String)



-- UPDATE


feedDataDecoder : String -> Cmd Msg
feedDataDecoder query =
    Http.get
        { url = "https://cowuv5904j.execute-api.ap-northeast-1.amazonaws.com/?format=json&tags=" ++ query
        , expect = Http.expectString GetText
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchFeed query ->
            ( { model | status = Just "Loading..." }, feedDataDecoder query )

        GetText (Ok result) ->
            withNone { model | status = Nothing, feedResult = decode result }

        GetText (Err error) ->
            withNone { model | status = Nothing, feedResult = Err (HE.errorToString error) }



-- VIEW


viewStatus : Model -> Html Msg
viewStatus model =
    case model.status of
        Just message ->
            span [] [ text message ]

        Nothing ->
            span [ displayNone ] []


viewInput : Model -> Html Msg
viewInput model =
    div []
        [ div [ class "inputline" ]
            [ span [] [ text "Search Flickr: " ]
            , input
                [ type_ "text"
                , placeholder "e.g. nymphalidae"
                , class "inputline__text"
                , onEnter FetchFeed
                ]
                []
            , viewStatus model
            ]
        ]


viewThumbnail : String -> Html Msg
viewThumbnail url =
    div
        [ class "ex49__thumbnail-container" ]
        [ img
            [ src url
            , alt "Flickr Image"
            , class "thumbnail"
            ]
            []
        ]


viewPictures : Model -> Html Msg
viewPictures model =
    case model.feedResult of
        Ok pictures ->
            div [ class "ex49__gallery" ]
                (List.map viewThumbnail pictures)

        Err error ->
            div [] [ text ("Error: " ++ error) ]


view : Model -> Html Msg
view model =
    div []
        [ viewInput model
        , viewPictures model
        ]
