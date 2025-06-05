module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Ex01 as Ex01
import Pages.Ex02 as Ex02
import Pages.Ex03 as Ex03
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), top)



-- MODEL


type alias Model =
    { key : Nav.Key
    , currentExercise : Maybe Int
    , ex01Model : Maybe Ex01.Model
    , ex02Model : Maybe Ex02.Model
    , ex03Model : Maybe Ex03.Model
    }


defaultModel : Nav.Key -> Model
defaultModel key =
    { key = key
    , currentExercise = Nothing
    , ex01Model = Nothing
    , ex02Model = Nothing
    , ex03Model = Nothing
    }



-- MSG


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Ex01Msg Ex01.Msg
    | Ex02Msg Ex02.Msg
    | Ex03Msg Ex03.Msg



-- ROUTE


type Route
    = Home
    | Exercise Int


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Home top
        , Parser.map Exercise exerciseParser
        ]


exerciseParser : Parser.Parser (Int -> a) a
exerciseParser =
    Parser.custom "EXERCISE" <|
        \segment ->
            if String.startsWith "ex" segment && String.length segment >= 3 then
                String.dropLeft 2 segment
                    |> String.toInt
                    |> Maybe.andThen
                        (\n ->
                            if n >= 1 && n <= 57 then
                                Just n

                            else
                                Nothing
                        )

            else
                Nothing



-- INIT


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            Parser.parse routeParser url |> Maybe.withDefault Home

        model0 =
            defaultModel key

        model =
            case route of
                Exercise 1 ->
                    { model0 | currentExercise = Just 1, ex01Model = Just Ex01.init }

                Exercise 2 ->
                    { model0 | currentExercise = Just 2, ex02Model = Just Ex02.init }

                Exercise 3 ->
                    { model0 | currentExercise = Just 3, ex03Model = Just Ex03.init }

                _ ->
                    model0
    in
    ( model, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                route =
                    Parser.parse routeParser url |> Maybe.withDefault Home

                model0 =
                    defaultModel model.key
            in
            case route of
                Exercise 1 ->
                    ( { model0 | currentExercise = Just 1, ex01Model = Just Ex01.init }, Cmd.none )

                Exercise 2 ->
                    ( { model0 | currentExercise = Just 2, ex02Model = Just Ex02.init }, Cmd.none )

                Exercise 3 ->
                    ( { model0 | currentExercise = Just 3, ex03Model = Just Ex03.init }, Cmd.none )

                _ ->
                    ( model0, Cmd.none )

        Ex01Msg subMsg ->
            case model.ex01Model of
                Just ex01Model ->
                    let
                        ( newModel, cmd ) =
                            Ex01.update subMsg ex01Model
                    in
                    ( { model | ex01Model = Just newModel }, Cmd.map Ex01Msg cmd )

                Nothing ->
                    ( model, Cmd.none )

        Ex02Msg subMsg ->
            case model.ex02Model of
                Just ex02Model ->
                    let
                        ( newModel, cmd ) =
                            Ex02.update subMsg ex02Model
                    in
                    ( { model | ex02Model = Just newModel }, Cmd.map Ex02Msg cmd )

                Nothing ->
                    ( model, Cmd.none )

        Ex03Msg subMsg ->
            case model.ex03Model of
                Just ex03Model ->
                    let
                        ( newModel, cmd ) =
                            Ex03.update subMsg ex03Model
                    in
                    ( { model | ex03Model = Just newModel }, Cmd.map Ex03Msg cmd )

                Nothing ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Elm 57 Exercises"
    , body =
        [ div [ style "display" "flex", style "height" "100vh" ]
            [ leftPane
            , mainPane model
            ]
        ]
    }


leftPane : Html Msg
leftPane =
    div [ style "width" "200px", style "background-color" "#f0f0f0", style "padding" "20px" ]
        [ h3 [] [ text "Exercises" ]
        , div [] (List.map exerciseLink (List.range 1 57))
        ]


exerciseLink : Int -> Html Msg
exerciseLink n =
    let
        paddedNumber =
            String.padLeft 2 '0' (String.fromInt n)
    in
    div [ style "margin" "5px 0" ]
        [ a [ href ("/ex" ++ paddedNumber) ] [ text ("Exercise " ++ paddedNumber) ]
        ]


mainPane : Model -> Html Msg
mainPane model =
    div [ style "flex" "1", style "padding" "20px" ]
        [ case model.currentExercise of
            Just 1 ->
                case model.ex01Model of
                    Just ex01Model ->
                        Html.map Ex01Msg (Ex01.view ex01Model)

                    Nothing ->
                        text "Loading Ex01..."

            Just 2 ->
                case model.ex02Model of
                    Just ex02Model ->
                        Html.map Ex02Msg (Ex02.view ex02Model)

                    Nothing ->
                        text "Loading Ex02..."

            Just 3 ->
                case model.ex03Model of
                    Just ex03Model ->
                        Html.map Ex03Msg (Ex03.view ex03Model)

                    Nothing ->
                        text "Loading Ex03..."

            Just n ->
                text ("Exercise " ++ String.fromInt n ++ " - Not implemented yet")

            Nothing ->
                div []
                    [ h1 [] [ text "Welcome to Elm 57 Exercises" ]
                    , p [] [ text "Select an exercise from the left panel." ]
                    ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
