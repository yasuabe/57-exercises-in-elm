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
import Html.Attributes exposing (alt, class, placeholder, src, title, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D exposing (errorToString)
import List.Extra as LX
import Maybe.Extra as MX
import Result exposing (Result)
import String as S exposing (dropLeft, dropRight)



-- MODEL


type alias Item =
    { title : String
    , url : String
    , date_taken : String
    , author : String
    }


type alias Model =
    { feedResult : Result String (List Item)
    , status : Maybe String
    , selected : Maybe Int
    }


init : Model
init =
    { feedResult = Ok []
    , status = Nothing
    , selected = Nothing
    }


decode : String -> Result String (List Item)
decode =
    let
        stripJsonp =
            dropLeft (S.length "jsonFlickrFeed(") >> dropRight (S.length ")")

        decoder =
            D.field "items" <|
                D.list <|
                    D.map4 Item
                        (D.field "title" D.string)
                        (D.at [ "media", "m" ] D.string)
                        (D.field "date_taken" D.string)
                        (D.field "author" D.string)
    in
    stripJsonp >> D.decodeString decoder >> Result.mapError errorToString


selectedItem : Model -> Maybe Item
selectedItem model =
    MX.andThen2
        (\index items -> LX.getAt index items)
        model.selected
        (Result.toMaybe model.feedResult)



-- MSG


type Msg
    = FetchFeed String
    | GetText (Result Http.Error String)
    | SelectImage Int
    | UnselectImage



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
            ( { model | selected = Nothing, status = Just "Loading..." }, feedDataDecoder query )

        GetText (Ok result) ->
            withNone { model | status = Nothing, feedResult = decode result }

        GetText (Err error) ->
            withNone { model | status = Nothing, feedResult = Err (HE.errorToString error) }

        SelectImage index ->
            ( { model | selected = Just index }, Cmd.none )

        UnselectImage ->
            ( { model | selected = Nothing }, Cmd.none )



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


viewThumbnail : Int -> Item -> Html Msg
viewThumbnail index item =
    div
        [ class "ex49__thumbnail-container" ]
        [ img
            [ src item.url
            , alt item.title
            , title item.title
            , onClick (SelectImage index)
            ]
            []
        ]


viewPictures : Model -> Html Msg
viewPictures model =
    case model.feedResult of
        Ok pictures ->
            div [ class "ex49__gallery" ]
                (List.indexedMap viewThumbnail pictures)

        Err error ->
            div [] [ text ("Error: " ++ error) ]


viewEnlarged : Model -> Html Msg
viewEnlarged model =
    case selectedItem model of
        Just item ->
            div
                [ class "ex49__enlarged-overlay"
                , onClick UnselectImage
                ]
                [ div
                    [ class "ex49__close-button"
                    , onClick UnselectImage
                    ]
                    [ text "×" ]
                , div
                    [ class "ex49__enlarged-content"
                    , onClick UnselectImage
                    ]
                    [ img [ src item.url, alt item.title, title item.title ] []
                    , div [ class "ex49__enlarged-title" ] [ text item.title ]
                    , div [ class "ex49__enlarged-date" ] [ text item.date_taken ]
                    , div [ class "ex49__enlarged-date" ] [ text item.author ]
                    ]
                ]

        Nothing ->
            div [ displayNone ] []


view : Model -> Html Msg
view model =
    div []
        [ viewInput model
        , div [ class "ex49__container" ]
            [ viewPictures model
            , viewEnlarged model
            ]
        ]
