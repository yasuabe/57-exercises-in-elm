module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Ex01 as Ex01
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), int, s, top)

-- MODEL

type alias Model =
    { key : Nav.Key
    , currentExercise : Maybe Int
    , ex01Model : Maybe Ex01.Model
    -- 必要に応じて他のExerciseのModelを追加
    }

-- MSG

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Ex01Msg Ex01.Msg

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
                    |> Maybe.andThen (\n -> if n >= 1 && n <= 57 then Just n else Nothing)
            else
                Nothing

-- INIT

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route = Parser.parse routeParser url |> Maybe.withDefault Home
        (currentEx, initModel) = case route of
            Exercise 1 -> (Just 1, Just Ex01.init)
            _ -> (Nothing, Nothing)
    in
    ( { key = key, currentExercise = currentEx, ex01Model = initModel }, Cmd.none )

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
                route = Parser.parse routeParser url |> Maybe.withDefault Home
            in
            case route of
                Exercise 1 ->
                    ( { model | currentExercise = Just 1, ex01Model = Just Ex01.init }, Cmd.none )
                _ ->
                    ( { model | currentExercise = Nothing, ex01Model = Nothing }, Cmd.none )

        Ex01Msg subMsg ->
            case model.ex01Model of
                Just ex01Model ->
                    let
                        (newModel, cmd) = Ex01.update subMsg ex01Model
                    in
                    ( { model | ex01Model = Just newModel }, Cmd.map Ex01Msg cmd )
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
        paddedNumber = String.padLeft 2 '0' (String.fromInt n)
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
subscriptions _ = Sub.none

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

