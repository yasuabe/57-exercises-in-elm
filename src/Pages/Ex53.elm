module Pages.Ex53 exposing (Ex53Command(..), Model, Msg(..), TodoRecord, init, subscriptions, update, view)

import Date as D
import Debug exposing (todo)
import Hour as T
import Html exposing (Html, button, div, input, table, tbody, td, text, th, tr)
import Html.Attributes exposing (class, placeholder, style, value, disabled)
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
    { inputTodo : String
    , todos : List TodoRecord
    }


init : ( Model, Cmd Msg, Maybe Ex53Command )
init =
    ( { inputTodo = ""
      , todos = []
      }
    , Cmd.none
    , Just ReadFromIndexedDB
    )



-- MSG


type Msg
    = Submit
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
        Submit ->
            ( model
            , Cmd.none
            , Just (WriteToIndexedDB <| model.inputTodo)
            )

        Delete todoId ->
            ( model
            , Cmd.none
            , Just (DeleteTodo todoId)
            )

        WriteComplete _ ->
            ( { model | inputTodo = "" }
            , Cmd.none
            , Just ReadFromIndexedDB
            )

        LoadComplete todos ->
            ( { model | todos = todos }
            , Cmd.none
            , Nothing
            )

        DeleteComplete _ ->
            ( model
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
        [ table [ class "todo-list-table" ]
            [ tbody []
                (tr []
                    [ th [] [ text "ID" ]
                    , th [ style "width" "70%" ] [ text "TODO" ]
                    , th [] [ text "Created" ]
                    , th [] [ text "" ]
                    ]
                    :: tr []
                        [ td [] []
                        , td []
                            [ input
                                [ placeholder "Enter a todo task"
                                , onInput InputChanged
                                , value model.inputTodo
                                , class "todo-input"
                                ]
                                []
                            ]
                        , td [] []
                        , td [] [ button
                                  [ class "add-button"
                                  , onClick Submit
                                  , disabled (String.trim model.inputTodo == "")
                                  ] [ text "Add" ] ]
                        ]
                    :: List.map
                        (\todo ->
                            tr []
                                [ td [] [ text (String.fromInt todo.id) ]
                                , td [] [ text todo.data ]
                                , td [] [ text <| convertDateTimeFormat todo.timestamp ]
                                , td [] [ button [ class "delete-button", onClick <| Delete todo.id ] [ text "Done" ] ]
                                ]
                        )
                        model.todos
                )
            ]
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
