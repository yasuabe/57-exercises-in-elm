module Main exposing (main)

import Array as A
import Browser
import Browser.Navigation as Nav
import Exercises exposing (Exercise, exercises, toTitle)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Ex01 as Ex01
import Pages.Ex02 as Ex02
import Pages.Ex03 as Ex03
import Pages.Ex04 as Ex04
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), top)



-- MODEL


type alias Model =
    { key : Nav.Key
    , currentExercise : Maybe Int
    , ex01Model : Maybe Ex01.Model
    , ex02Model : Maybe Ex02.Model
    , ex03Model : Maybe Ex03.Model
    , ex04Model : Maybe Ex04.Model
    }


defaultModel : Nav.Key -> Model
defaultModel key =
    { key = key
    , currentExercise = Nothing
    , ex01Model = Nothing
    , ex02Model = Nothing
    , ex03Model = Nothing
    , ex04Model = Nothing
    }



-- MSG


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Ex01Msg Ex01.Msg
    | Ex02Msg Ex02.Msg
    | Ex03Msg Ex03.Msg
    | Ex04Msg Ex04.Msg



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

                Exercise 4 ->
                    { model0 | currentExercise = Just 4, ex04Model = Just Ex04.init }

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

                Exercise 4 ->
                    ( { model0 | currentExercise = Just 4, ex04Model = Just Ex04.init }, Cmd.none )

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

        Ex04Msg subMsg ->
            case model.ex04Model of
                Just ex04Model ->
                    let
                        ( newModel, cmd ) =
                            Ex04.update subMsg ex04Model
                    in
                    ( { model | ex04Model = Just newModel }, Cmd.map Ex04Msg cmd )

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
    div [ style "width" "300px", style "background-color" "#f0f0f0", style "padding" "20px" ]
        [ h3 [] [ text "Exercises" ]
        , div [] (List.map exerciseLink2 exercises)
        ]


exerciseLink2 : Exercise -> Html Msg
exerciseLink2 e =
    let
        title =
            text <| toTitle e
    in
    div [ style "margin" "5px 0" ]
        [ if e.done then
            a [ href ("/ex" ++ e.suffix) ] [ title ]

          else
            title
        ]


mapMain : Model -> Int -> (Model -> Maybe a) -> (a -> Html msg) -> (msg -> Msg) -> Html Msg
mapMain model n g h msg =
    let
        exercise =
            exerciseAt n
    in
    case g model of
        Just exModel ->
            div []
                [ h1 [] [ text <| toTitle exercise ]
                , Html.map msg (h exModel)
                ]

        Nothing ->
            text <| "Loading " ++ exercise.title


exerciseAt : Int -> Exercise
exerciseAt n =
    case A.get (n - 1) (A.fromList exercises) of
        Just exercise ->
            exercise

        Nothing ->
            { suffix = String.fromInt n, title = "Exercise " ++ String.fromInt n, done = False }


mainPane : Model -> Html Msg
mainPane model =
    div [ style "flex" "1", style "padding" "20px" ]
        [ case model.currentExercise of
            Just 1 ->
                mapMain model 1 (\m -> m.ex01Model) Ex01.view Ex01Msg

            Just 2 ->
                mapMain model 2 (\m -> m.ex02Model) Ex02.view Ex02Msg

            Just 3 ->
                mapMain model 3 (\m -> m.ex03Model) Ex03.view Ex03Msg

            Just 4 ->
                mapMain model 4 (\m -> m.ex04Model) Ex04.view Ex04Msg

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
