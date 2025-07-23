-- Ex41: Name Sorter
--
-- - Read a list of names from a file.
-- - Sort the names alphabetically.
-- - Output:
--    - Total number of names.
--    - A separator line.
--    - The sorted names.
-- - Do not hard-code the number of names.


module Pages.Ex41 exposing (Model, Msg(..), init, update, view)

import File exposing (File)
import File.Download as Download
import File.Select as Select
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
    | SortNames String
    | DownloadSorted String



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

        SortNames content ->
            ( { model | sorted = Just <| makeSortedContent content }, Cmd.none )

        DownloadSorted sorted ->
            ( model
            , Download.string "sorted.txt" "text/plain" sorted
            )


makeSortedContent : String -> String
makeSortedContent content =
    let
        lines =
            String.split "\n" content
                |> List.filter ((/=) "")
                |> List.map String.trim
                |> List.sort

        total =
            String.fromInt (List.length lines)

        header =
            "Total: " ++ total ++ " Names"

        separator =
            "-----------------"
    in
    header
        :: separator
        :: lines
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
    , button [ onClick (SortNames content) ] [ text "Sort Names" ]
    ]


viewSortedContent : String -> List (Html Msg)
viewSortedContent sorted =
    [ pre [] [ text sorted ]
    , button [ onClick (DownloadSorted sorted) ] [ text "Download Sorted Names" ]
    ]
