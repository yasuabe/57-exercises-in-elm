module Pages.Ex46 exposing (Model, Msg(..), countWords, init, update, view)

import Basics.Extra exposing (flip)
import Common.CmdEx exposing (withNone)
import Common.Function exposing (on)
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, span, text)
import Html.Attributes as A exposing (class)
import Html.Events exposing (onClick)
import List.Extra as LX exposing (count)
import Maybe
import Maybe.Extra as MX
import Task
import Tuple exposing (second)



-- MODEL


defaultContents : String
defaultContents =
    "badger badger badger badger mushroom mushroom snake badger badger badger"


type alias Model =
    { contents : String }


init : Model
init =
    { contents = defaultContents }


countWords : Model -> List ( String, Int )
countWords model =
    let
        countUp : String -> Dict String Int -> Dict String Int
        countUp word =
            Dict.update word (Maybe.map ((+) 1) >> flip MX.or (Just 1))
    in
    model.contents
        |> String.split "\n"
        |> List.concatMap String.words
        |> List.foldl countUp Dict.empty
        |> Dict.toList
        |> List.sortWith (on (flip compare) second)



-- MSG


type Msg
    = Submit
    | FileSelected File
    | FileLoaded String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
            ( model, Select.file [ "text/plain" ] FileSelected )

        FileSelected file ->
            ( model, Task.perform FileLoaded (File.toString file) )

        FileLoaded contents_ ->
            withNone { model | contents = contents_ }



-- VIEW


viewHistgram : List ( String, Int ) -> Html Msg
viewHistgram hist =
    let
        maxCount =
            hist
                |> LX.maximumBy Tuple.second
                |> MX.unwrap 0 Tuple.second

        line ( word, count ) =
            div []
                [ span [] [ text word ]
                , span [ A.style "width" (String.fromInt (count * 100 // maxCount) ++ "%") ] []
                , span [ A.style "margin-left" "8px" ] [ text (String.fromInt count) ]
                ]
    in
    div [ class "ex46__histogram" ] (List.map line hist)


view : Model -> Html Msg
view model =
    div [ class "ex46" ]
        [ div [] [ button [ onClick Submit ] [ text "Upload" ] ]
        , div [ class "output" ] [ viewHistgram (countWords model) ]
        ]
