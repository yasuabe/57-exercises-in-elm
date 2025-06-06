module Pages.Ex53 exposing (Ex53Command(..), Model, Msg(..), TodoRecord, init, subscriptions, update, view)

import Date as D
import Debug exposing (todo)
import Hour as T
import Html exposing (Html, button, div, input, table, tbody, td, text, th, tr)
import Html.Attributes exposing (placeholder, style)
import Html.Events exposing (onClick, onInput)
import Result exposing (toMaybe)
import String exposing (dropRight)



-- MODEL


type alias TodoRecord =
    { id : Int
    , data : String
    , timestamp : String
    }


type alias Model =
    { status : String
    , inputTodo : String
    , todos : List TodoRecord
    }


init : ( Model, Cmd Msg, Maybe Ex53Command )
init =
    ( { status = "準備中..."
      , inputTodo = ""
      , todos = []
      }
    , Cmd.none
    , Just ReadFromIndexedDB
    )



-- MSG


type Msg
    = LoadData
    | Submit
    | Delete Int
    | LoadComplete (List TodoRecord)
    | WriteComplete String
    | DeleteComplete String
    | InputChanged String


type Ex53Command
    = ReadFromIndexedDB
    | WriteToIndexedDB String
    | DeleteTodo Int



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Ex53Command )
update msg model =
    case msg of
        LoadData ->
            ( { model | status = "データ読み込み中..." }
            , Cmd.none
            , Just ReadFromIndexedDB
            )

        Submit ->
            ( { model | status = "書き込み中..." }
            , Cmd.none
            , Just (WriteToIndexedDB <| model.inputTodo)
            )

        Delete todoId ->
            ( { model | status = "Deleting..." }
            , Cmd.none
            , Just (DeleteTodo todoId)
            )

        WriteComplete result ->
            ( { model | status = "完了: " ++ result }
            , Cmd.none
            , Just ReadFromIndexedDB
            )

        LoadComplete todos ->
            ( { model | status = "読み込み完了", todos = todos }
            , Cmd.none
            , Nothing
            )

        DeleteComplete result ->
            ( { model | status = "deleted: " ++ result }
            , Cmd.none
            , Just ReadFromIndexedDB
            )

        InputChanged str ->
            ( { model | inputTodo = str }
            , Cmd.none
            , Nothing
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ table []
            [ tbody []
                (tr []
                    [ th [] [ text "ID" ]
                    , th [ style "width" "70%" ] [ text "TODO" ]
                    , th [] [ text "timestamp" ]
                    , th [] [ text "ops." ]
                    ]
                    :: tr []
                        [ th [] []
                        , th []
                            [ input
                                [ placeholder "Enter a noun"
                                , onInput InputChanged
                                , style "width" "100%"
                                ]
                                []
                            ]
                        , th [] []
                        , th [] [ button [ onClick Submit ] [ text "Submit" ] ]
                        ]
                    :: List.map
                        (\todo ->
                            tr []
                                [ td [] [ text (String.fromInt todo.id) ]
                                , td [] [ text todo.data ]
                                , td [] [ text <| convertDateTimeFormat todo.timestamp ]
                                , td [] [ button [ onClick <| Delete todo.id ] [ text "delete" ] ]
                                ]
                        )
                        model.todos
                )
            ]
        , div [] [ text ("ステータス: " ++ model.status) ]
        , button [ onClick LoadData ] [ text "read from IndexedDB" ]
        ]


convertDateTimeFormat : String -> String
convertDateTimeFormat isoString =
    case String.split "T" isoString of
        [ date, time ] ->
            let
                mbDate =
                    Maybe.map (\x -> D.format "MM/dd" x) <| toMaybe <| D.fromIsoString date

                mbTime =
                    Maybe.map (\x -> T.format "hh:mm" x) <| T.fromIsoString <| dropRight 1 time
            in
            Maybe.map2 (\d t -> d ++ " " ++ t) mbDate mbTime
                |> Maybe.withDefault "---"

        _ ->
            todo "Not implemented"
