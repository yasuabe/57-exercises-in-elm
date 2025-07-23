module Pages.Ex41 exposing (Model, Msg(..), init, update, view)

import File exposing (File)
import File.Select as Select
import File.Download as Download
import Html exposing (Html, button, div, pre, text)
import Html.Events exposing (onClick)
import Maybe exposing (withDefault)
import Task



-- MODEL


type alias Model =
    { source : Maybe String
    , sorted : Maybe String
    }


init : Model
init =
    { source = Nothing
    , sorted = Nothing
    }



-- MSG


type Msg
    = NameCsvRequested
    | GotFiles File
    | FileLoaded String
    | Sort String
    | Download String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameCsvRequested ->
            ( model, Select.file [ "text/plain" ] GotFiles )

        GotFiles file ->
            ( model, Task.perform FileLoaded (File.toString file) )

        FileLoaded content ->
            ( { model | source = Just content }, Cmd.none )

        Sort content ->
            ( {model| sorted = Just <| makeSortedContent content}, Cmd.none )

        Download sorted ->
            ( model,
                Download.string "sorted.txt" "text/plain" sorted
             )


makeSortedContent : String -> String
makeSortedContent content =
    let
        lines =
            String.split "\n" content
                |> List.filter ((/=) "")
                |> List.map String.trim
                |> List.sort
        total = String.fromInt (List.length lines)
        header = "Total of " ++ total ++ " Names"
        separator = "-----------------"
    in
    [header, separator]
        ++ lines
        |> String.join "\n"

-- VIEW


view : Model -> Html Msg
view model =
    div []
        (button [ onClick NameCsvRequested ] [ text "Load Names File" ]
            :: (Maybe.map viewSourceFile model.source |> withDefault [])
            ++ (Maybe.map viewSortedContent model.sorted |> withDefault [])
        )


viewSourceFile : String -> List (Html Msg)
viewSourceFile content =
    [ pre [] [ text content ]
    , button [ onClick (Sort content) ] [ text "Sort" ]
    ]


viewSortedContent : String -> List (Html Msg)
viewSortedContent sorted =
    [ pre [] [ text sorted ]
    , button [ onClick (Download sorted) ] [ text "Download" ]
    ]
