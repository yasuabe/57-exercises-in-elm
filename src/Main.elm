port module Main exposing (main)

import Array as A
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Exercises exposing (Exercise, chapters, exercises, toTitle)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (withDefault)
import Pages.Ex01 as Ex01
import Pages.Ex02 as Ex02
import Pages.Ex03 as Ex03
import Pages.Ex04 as Ex04
import Pages.Ex05 as Ex05
import Pages.Ex06 as Ex06
import Pages.Ex07 as Ex07
import Pages.Ex08 as Ex08
import Pages.Ex09 as Ex09
import Pages.Ex10 as Ex10
import Pages.Ex11 as Ex11
import Pages.Ex12 as Ex12
import Pages.Ex13 as Ex13
import Pages.Ex14 as Ex14
import Pages.Ex15 as Ex15
import Pages.Ex16 as Ex16
import Pages.Ex17 as Ex17
import Pages.Ex18 as Ex18
import Pages.Ex19 as Ex19
import Pages.Ex20 as Ex20
import Pages.Ex21 as Ex21
import Pages.Ex22 as Ex22
import Pages.Ex23 as Ex23
import Pages.Ex24 as Ex24
import Pages.Ex25 as Ex25
import Pages.Ex26 as Ex26
import Pages.Ex27 as Ex27
import Pages.Ex28 as Ex28
import Pages.Ex29 as Ex29
import Pages.Ex30 as Ex30
import Pages.Ex31 as Ex31
import Pages.Ex32 as Ex32
import Pages.Ex33 as Ex33
import Pages.Ex34 as Ex34
import Pages.Ex35 as Ex35
import Pages.Ex36 as Ex36
import Pages.Ex37 as Ex37
import Pages.Ex38 as Ex38
import Pages.Ex39 as Ex39
import Pages.Ex40 as Ex40
import Pages.Ex41 as Ex41
import Pages.Ex42 as Ex42
import Pages.Ex43 as Ex43
import Pages.Ex44 as Ex44
import Pages.Ex45 as Ex45
import Pages.Ex46 as Ex46
import Pages.Ex47 as Ex47
import Pages.Ex48 as Ex48
import Pages.Ex49 as Ex49
import Pages.Ex50 as Ex50
import Pages.Ex51 as Ex51
import Pages.Ex52 as Ex52
import Pages.Ex53 as Ex53
import Pages.Ex54 as Ex54
import Pages.Ex55 as Ex55
import Pages.Ex56 as Ex56
import Pages.Ex57 as Ex57
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), top)


port writeToIndexedDB : String -> Cmd msg


port readFromIndexedDB : () -> Cmd msg


port deleteTodo : Int -> Cmd msg


port indexedDBResult : (String -> msg) -> Sub msg


port indexedDBReadResult : (List Ex53.TodoRecord -> msg) -> Sub msg


port deleteTodoResult : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
    { key : Nav.Key
    , currentExercise : Maybe Int
    , ex01Model : Maybe Ex01.Model
    , ex02Model : Maybe Ex02.Model
    , ex03Model : Maybe Ex03.Model
    , ex04Model : Maybe Ex04.Model
    , ex05Model : Maybe Ex05.Model
    , ex06Model : Maybe Ex06.Model
    , ex07Model : Maybe Ex07.Model
    , ex08Model : Maybe Ex08.Model
    , ex09Model : Maybe Ex09.Model
    , ex10Model : Maybe Ex10.Model
    , ex11Model : Maybe Ex11.Model
    , ex12Model : Maybe Ex12.Model
    , ex13Model : Maybe Ex13.Model
    , ex14Model : Maybe Ex14.Model
    , ex15Model : Maybe Ex15.Model
    , ex16Model : Maybe Ex16.Model
    , ex17Model : Maybe Ex17.Model
    , ex18Model : Maybe Ex18.Model
    , ex19Model : Maybe Ex19.Model
    , ex20Model : Maybe Ex20.Model
    , ex21Model : Maybe Ex21.Model
    , ex22Model : Maybe Ex22.Model
    , ex23Model : Maybe Ex23.Model
    , ex24Model : Maybe Ex24.Model
    , ex25Model : Maybe Ex25.Model
    , ex26Model : Maybe Ex26.Model
    , ex27Model : Maybe Ex27.Model
    , ex28Model : Maybe Ex28.Model
    , ex29Model : Maybe Ex29.Model
    , ex30Model : Maybe Ex30.Model
    , ex31Model : Maybe Ex31.Model
    , ex32Model : Maybe Ex32.Model
    , ex33Model : Maybe Ex33.Model
    , ex34Model : Maybe Ex34.Model
    , ex35Model : Maybe Ex35.Model
    , ex36Model : Maybe Ex36.Model
    , ex37Model : Maybe Ex37.Model
    , ex38Model : Maybe Ex38.Model
    , ex39Model : Maybe Ex39.Model
    , ex40Model : Maybe Ex40.Model
    , ex41Model : Maybe Ex41.Model
    , ex42Model : Maybe Ex42.Model
    , ex43Model : Maybe Ex43.Model
    , ex44Model : Maybe Ex44.Model
    , ex45Model : Maybe Ex45.Model
    , ex46Model : Maybe Ex46.Model
    , ex47Model : Maybe Ex47.Model
    , ex48Model : Maybe Ex48.Model
    , ex49Model : Maybe Ex49.Model
    , ex50Model : Maybe Ex50.Model
    , ex51Model : Maybe Ex51.Model
    , ex52Model : Maybe Ex52.Model
    , ex53Model : Maybe Ex53.Model
    , ex54Model : Maybe Ex54.Model
    , ex55Model : Maybe Ex55.Model
    , ex56Model : Maybe Ex56.Model
    , ex57Model : Maybe Ex57.Model
    }


defaultModel : Nav.Key -> Model
defaultModel key =
    { key = key
    , currentExercise = Nothing
    , ex01Model = Nothing
    , ex02Model = Nothing
    , ex03Model = Nothing
    , ex04Model = Nothing
    , ex05Model = Nothing
    , ex06Model = Nothing
    , ex07Model = Nothing
    , ex08Model = Nothing
    , ex09Model = Nothing
    , ex10Model = Nothing
    , ex11Model = Nothing
    , ex12Model = Nothing
    , ex13Model = Nothing
    , ex14Model = Nothing
    , ex15Model = Nothing
    , ex16Model = Nothing
    , ex17Model = Nothing
    , ex18Model = Nothing
    , ex19Model = Nothing
    , ex20Model = Nothing
    , ex21Model = Nothing
    , ex22Model = Nothing
    , ex23Model = Nothing
    , ex24Model = Nothing
    , ex25Model = Nothing
    , ex26Model = Nothing
    , ex27Model = Nothing
    , ex28Model = Nothing
    , ex29Model = Nothing
    , ex30Model = Nothing
    , ex31Model = Nothing
    , ex32Model = Nothing
    , ex33Model = Nothing
    , ex34Model = Nothing
    , ex35Model = Nothing
    , ex36Model = Nothing
    , ex37Model = Nothing
    , ex38Model = Nothing
    , ex39Model = Nothing
    , ex40Model = Nothing
    , ex41Model = Nothing
    , ex42Model = Nothing
    , ex43Model = Nothing
    , ex44Model = Nothing
    , ex45Model = Nothing
    , ex46Model = Nothing
    , ex47Model = Nothing
    , ex48Model = Nothing
    , ex49Model = Nothing
    , ex50Model = Nothing
    , ex51Model = Nothing
    , ex52Model = Nothing
    , ex53Model = Nothing
    , ex54Model = Nothing
    , ex55Model = Nothing
    , ex56Model = Nothing
    , ex57Model = Nothing
    }


type alias Params =
    { init : Model -> Model
    }


initDict : Dict Int Params
initDict =
    Dict.fromList
        [ ( 1, { init = \m -> { m | ex01Model = Just Ex01.init } } )
        , ( 2, { init = \m -> { m | ex02Model = Just Ex02.init } } )
        , ( 3, { init = \m -> { m | ex03Model = Just Ex03.init } } )
        , ( 4, { init = \m -> { m | ex04Model = Just Ex04.init } } )
        , ( 7, { init = \m -> { m | ex07Model = Just Ex07.init } } )
        , ( 13, { init = \m -> { m | ex13Model = Just Ex13.init } } )
        , ( 14, { init = \m -> { m | ex14Model = Just Ex14.init } } )
        , ( 23, { init = \m -> { m | ex23Model = Just Ex23.init } } )
        , ( 24, { init = \m -> { m | ex24Model = Just Ex24.init } } )
        , ( 28, { init = \m -> { m | ex28Model = Just Ex28.init } } )
        , ( 33, { init = \m -> { m | ex33Model = Just Ex33.init } } )
        , ( 41, { init = \m -> { m | ex41Model = Just Ex41.init } } )
        , ( 47, { init = \m -> { m | ex47Model = Just Ex47.init } } )
        ]



-- MSG


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Ex01Msg Ex01.Msg
    | Ex02Msg Ex02.Msg
    | Ex03Msg Ex03.Msg
    | Ex04Msg Ex04.Msg
    | Ex07Msg Ex07.Msg
    | Ex13Msg Ex13.Msg
    | Ex14Msg Ex14.Msg
    | Ex23Msg Ex23.Msg
    | Ex24Msg Ex24.Msg
    | Ex28Msg Ex28.Msg
    | Ex33Msg Ex33.Msg
    | Ex41Msg Ex41.Msg
    | Ex47Msg Ex47.Msg
    | Ex53Msg Ex53.Msg



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
                Exercise n ->
                    Dict.get n initDict
                        |> Maybe.map (\p -> p.init { model0 | currentExercise = Just n })
                        |> withDefault model0

                Home ->
                    model0
    in
    ( model, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        update_ getter subMsg updator setter mkMsg =
            case getter model of
                Just submodel ->
                    let
                        ( newModel, newCmd ) =
                            updator subMsg submodel
                    in
                    ( setter model newModel, Cmd.map mkMsg newCmd )

                Nothing ->
                    ( model, Cmd.none )
    in
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
                Exercise 53 ->
                    let
                        ( ex53Model, cmd, mbCommand ) =
                            Ex53.init

                        portCmd =
                            case mbCommand of
                                Just (Ex53.WriteToIndexedDB data) ->
                                    writeToIndexedDB data

                                Just (Ex53.DeleteTodo todoId) ->
                                    deleteTodo todoId

                                Just Ex53.ReadFromIndexedDB ->
                                    readFromIndexedDB ()

                                Nothing ->
                                    Cmd.none
                    in
                    ( { model0 | currentExercise = Just 53, ex53Model = Just ex53Model }
                    , Cmd.batch [ Cmd.map Ex53Msg cmd, portCmd ]
                    )

                Exercise n ->
                    ( Dict.get n initDict
                        |> Maybe.map (\p -> p.init { model0 | currentExercise = Just n })
                        |> withDefault model0
                    , Cmd.none
                    )

                Home ->
                    ( model0, Cmd.none )

        Ex01Msg subMsg ->
            update_ (\m -> m.ex01Model) subMsg Ex01.update (\m n -> { m | ex01Model = Just n }) Ex01Msg

        Ex02Msg subMsg ->
            update_ (\m -> m.ex02Model) subMsg Ex02.update (\m n -> { m | ex02Model = Just n }) Ex02Msg

        Ex03Msg subMsg ->
            update_ (\m -> m.ex03Model) subMsg Ex03.update (\m n -> { m | ex03Model = Just n }) Ex03Msg

        Ex04Msg subMsg ->
            update_ (\m -> m.ex04Model) subMsg Ex04.update (\m n -> { m | ex04Model = Just n }) Ex04Msg

        Ex07Msg subMsg ->
            update_ (\m -> m.ex07Model) subMsg Ex07.update (\m n -> { m | ex07Model = Just n }) Ex07Msg

        Ex13Msg subMsg ->
            update_ (\m -> m.ex13Model) subMsg Ex13.update (\m n -> { m | ex13Model = Just n }) Ex13Msg

        Ex14Msg subMsg ->
            update_ (\m -> m.ex14Model) subMsg Ex14.update (\m n -> { m | ex14Model = Just n }) Ex14Msg

        Ex23Msg subMsg ->
            update_ (\m -> m.ex23Model) subMsg Ex23.update (\m n -> { m | ex23Model = Just n }) Ex23Msg

        Ex24Msg subMsg ->
            update_ (\m -> m.ex24Model) subMsg Ex24.update (\m n -> { m | ex24Model = Just n }) Ex24Msg

        Ex28Msg subMsg ->
            update_ (\m -> m.ex28Model) subMsg Ex28.update (\m n -> { m | ex28Model = Just n }) Ex28Msg

        Ex33Msg subMsg ->
            update_ (\m -> m.ex33Model) subMsg Ex33.update (\m n -> { m | ex33Model = Just n }) Ex33Msg

        Ex41Msg subMsg ->
            update_ (\m -> m.ex41Model) subMsg Ex41.update (\m n -> { m | ex41Model = Just n }) Ex41Msg

        Ex47Msg subMsg ->
            update_ (\m -> m.ex47Model) subMsg Ex47.update (\m n -> { m | ex47Model = Just n }) Ex47Msg

        Ex53Msg subMsg ->
            case model.ex53Model of
                Just ex53Model ->
                    let
                        ( newModel, cmd, maybeCommand ) =
                            Ex53.update subMsg ex53Model

                        portCmd =
                            case maybeCommand of
                                Just (Ex53.WriteToIndexedDB data) ->
                                    writeToIndexedDB data

                                Just Ex53.ReadFromIndexedDB ->
                                    readFromIndexedDB ()

                                Just (Ex53.DeleteTodo todoId) ->
                                    deleteTodo todoId

                                Nothing ->
                                    Cmd.none
                    in
                    ( { model | ex53Model = Just newModel }
                    , Cmd.batch [ Cmd.map Ex53Msg cmd, portCmd ]
                    )

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
    div [ class "left-pane" ]
        [ h3 [] [ text "Exercises" ]
        , hr [] []
        , div [ class "left-pane__titles" ] (List.concatMap exerciseLink exercises)
        ]


exerciseLink : Exercise -> List (Html Msg)
exerciseLink e =
    let
        title =
            text <| toTitle e

        chapter =
            Dict.get e.suffix chapters
                |> Maybe.map (\c -> [ div [ class "left-pane__chapter-title" ] [ text c ] ])
                |> Maybe.withDefault []
    in
    chapter
        ++ [ div [ class "left-pane__exercise-title" ]
                [ if e.done then
                    a [ href ("/ex" ++ e.suffix) ] [ title ]

                  else
                    title
                ]
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

            Just 7 ->
                mapMain model 7 (\m -> m.ex07Model) Ex07.view Ex07Msg

            Just 13 ->
                mapMain model 13 (\m -> m.ex13Model) Ex13.view Ex13Msg

            Just 14 ->
                mapMain model 14 (\m -> m.ex14Model) Ex14.view Ex14Msg

            Just 23 ->
                mapMain model 23 (\m -> m.ex23Model) Ex23.view Ex23Msg

            Just 24 ->
                mapMain model 24 (\m -> m.ex24Model) Ex24.view Ex24Msg

            Just 28 ->
                mapMain model 28 (\m -> m.ex28Model) Ex28.view Ex28Msg

            Just 41 ->
                mapMain model 41 (\m -> m.ex41Model) Ex41.view Ex41Msg

            Just 47 ->
                mapMain model 47 (\m -> m.ex47Model) Ex47.view Ex47Msg

            Just 53 ->
                mapMain model 53 (\m -> m.ex53Model) Ex53.view Ex53Msg

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
subscriptions model =
    case model.currentExercise of
        Just 53 ->
            Sub.batch
                [ indexedDBResult (Ex53Msg << Ex53.WriteComplete)
                , indexedDBReadResult (Ex53Msg << Ex53.LoadComplete)
                , deleteTodoResult (Ex53Msg << Ex53.DeleteComplete)
                , case model.ex53Model of
                    Just ex53Model ->
                        Sub.map Ex53Msg (Ex53.subscriptions ex53Model)

                    Nothing ->
                        Sub.none
                ]

        _ ->
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
