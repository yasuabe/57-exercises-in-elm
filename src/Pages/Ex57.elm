module Pages.Ex57 exposing (Model, Msg(..), init, update, view)

import Array
import Html exposing (Html, button, div, input, label, li, ol, text)
import Html.Attributes exposing (checked, class, disabled, for, id, name, type_, value)
import Html.Events exposing (onClick)
import List.Extra as LE
import Maybe
import Maybe.Extra exposing (isNothing)
import Random exposing (generate)
import Random.List exposing (shuffle)
import Task



-- MODEL


type alias CurrentQuestion =
    ( Int, List Int )


type Model
    = NotStarted
    | InProgress CurrentQuestion ( List Int, List Int ) (Maybe Int)
    | Answered CurrentQuestion ( List Int, List Int ) Int
    | Finished ( List Int, List Int )


init : Model
init =
    NotStarted



-- MSG


type Msg
    = Shuffle
    | ShuffleComplete (List Int)
    | Distract ( List Int, List Int )
    | DistractComplete ( List Int, List Int ) (List Int)
    | OptionSelected Int
    | Submit
    | Continue



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Shuffle, _ ) ->
            ( init, generate ShuffleComplete (shuffle <| List.range 0 4) )

        ( ShuffleComplete shuffled, _ ) ->
            onShuffleComplete shuffled

        ( Distract sequence, _ ) ->
            onDistract model sequence

        ( DistractComplete sequence distractors, _ ) ->
            onDistractComplete sequence distractors model

        ( OptionSelected number, InProgress current qs _ ) ->
            onOptionSelected number current qs

        ( Submit, InProgress current qs (Just selection) ) ->
            ( Answered current qs selection, Cmd.none )

        ( Continue, Answered ( cur, _ ) sequence selected ) ->
            onContinue sequence cur selected

        ( _, _ ) ->
            ( model, Cmd.none )


onShuffleComplete : List Int -> ( Model, Cmd Msg )
onShuffleComplete shuffled =
    ( init, Task.perform Distract (Task.succeed ( shuffled, [] )) )


onDistract : Model -> ( List Int, List Int ) -> ( Model, Cmd Msg )
onDistract model questions =
    case questions of
        ( _ :: _, _ ) ->
            ( model, generate (DistractComplete questions) (shuffle (List.range 0 3)) )

        _ ->
            ( model, Cmd.none )


onDistractComplete : ( List Int, List Int ) -> List Int -> Model -> ( Model, Cmd Msg )
onDistractComplete ( sequence, solved ) distractors model =
    case sequence of
        q :: qs ->
            ( InProgress ( q, distractors ) ( qs, solved ) Nothing
            , Cmd.none
            )

        [] ->
            -- TODO: check if this is really needed
            ( model, Cmd.none )


onOptionSelected : Int -> CurrentQuestion -> ( List Int, List Int ) -> ( Model, Cmd Msg )
onOptionSelected number current qs =
    ( InProgress current qs (Just number), Cmd.none )


onContinue : ( List Int, List Int ) -> Int -> Int -> ( Model, Cmd Msg )
onContinue ( sequence, solved ) cur selected =
    case ( sequence, selected ) of
        ( _ :: _, 0 ) ->
            ( init, Task.perform Distract (Task.succeed ( sequence, cur :: solved )) )

        ( _ :: _, _ ) ->
            ( Finished ( cur :: sequence, solved ), Cmd.none )

        ( [], _ ) ->
            ( Finished ( [], cur :: solved ), Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        NotStarted ->
            viewInitialScreen

        InProgress ( q, distractors ) _ selection ->
            viewShowQuestion q distractors selection

        Answered ( q, distractors ) _ selection ->
            viewShowResult ( q, distractors, selection )

        Finished questions ->
            viewEndSession questions


viewInitialScreen : Html Msg
viewInitialScreen =
    div []
        [ div [ class "inputline" ]
            [ text "Click to start the trivia session" ]
        , div []
            [ Html.button [ onClick Shuffle ] [ text "Start" ] ]
        ]


viewShowQuestion : Int -> List Int -> Maybe Int -> Html Msg
viewShowQuestion questionNumber shuffledDistractors selection =
    let
        x =
            Array.get questionNumber triviaData
                |> Maybe.map
                    (\{ question, correct, distractors } ->
                        LE.zip (List.range 0 3) (correct :: distractors)
                            |> LE.zip shuffledDistractors
                            |> List.sortBy Tuple.first
                            |> List.map Tuple.second
                            |> (\l -> ( question, l ))
                    )
    in
    -- TODO: rename `x` to something more descriptive
    case x of
        Nothing ->
            div [] [ text "No trivia data available." ]

        Just ( question, options ) ->
            div []
                [ text ("Question: " ++ question)
                , div []
                    [ ol []
                        (List.map
                            (\( number, optionText ) ->
                                let
                                    index =
                                        String.fromInt number
                                in
                                li []
                                    [ input
                                        [ id index
                                        , type_ "radio"
                                        , name "options"
                                        , value index
                                        , checked (Just number == selection)
                                        , onClick (OptionSelected number)
                                        ]
                                        []
                                    , label [ for index ] [ text optionText ]
                                    ]
                            )
                            options
                        )
                    , button [ onClick Submit, disabled (isNothing selection) ]
                        [ text "Submit" ]
                    ]
                ]


viewShowResult : ( Int, List Int, Int ) -> Html Msg
viewShowResult ( questionNumber, shuffledDistractors, selection ) =
    let
        x =
            Array.get questionNumber triviaData
                |> Maybe.map
                    (\{ question, correct, distractors } ->
                        LE.zip (List.range 0 3) (correct :: distractors)
                            |> LE.zip shuffledDistractors
                            |> List.sortBy Tuple.first
                            |> List.map Tuple.second
                            |> (\l -> ( question, l ))
                    )
    in
    -- TODO: rename `x` to something more descriptive
    case x of
        Nothing ->
            div [] [ text "No trivia data available." ]

        Just ( question, options ) ->
            div []
                [ text ("Question: " ++ question)
                , div []
                    [ ol []
                        (List.map
                            (\( number, optionText ) ->
                                -- TODO: too deeply nested, consider refactoring
                                let
                                    index =
                                        String.fromInt number

                                    class_ =
                                        if number == selection then
                                            if number == 0 then
                                                "ex57__correct-answer"

                                            else
                                                "ex57__wrong-answer"

                                        else if number == 0 then
                                            "ex57__missed-correct-answer"

                                        else
                                            "ex57__disabled-text"
                                in
                                li [ class class_ ]
                                    [ input
                                        [ id index
                                        , type_ "radio"
                                        , name "options"
                                        , value index
                                        , disabled True
                                        , checked (number == selection)
                                        , onClick (OptionSelected number)
                                        ]
                                        []
                                    , label [ for index ] [ text optionText ]
                                    ]
                            )
                            options
                        )
                    , button [ onClick Continue ] [ text "Continue" ]
                    ]
                ]


viewResultList : List Int -> List Int -> Html Msg
viewResultList rest solved =
    let
        greens =
            List.repeat (List.length solved) "ex57__correct-question"

        classes =
            if List.isEmpty rest then
                greens

            else
                greens ++ ("ex57__wrong-question" :: List.repeat (List.length rest - 1) "ex57__disabled-text")

        questions =
            solved
                ++ rest
                |> List.filterMap (\n -> Array.get n triviaData |> Maybe.map .question)
                |> LE.zip classes
    in
    questions
        |> List.map
            (\(class_,  question  ) ->
                li [ class class_ ] [ text question ]
            )
        |> ol []


viewEndSession : ( List Int, List Int ) -> Html Msg
viewEndSession ( rest, solved ) =
    let
        resultList =
            viewResultList rest solved
    in
    case rest of
        [] ->
            div []
                [ resultList
                , text "Congratulations! You answered all questions correctly."
                , button [ onClick Shuffle ] [ text "Play Again" ]
                ]

        r :: rs ->
            div [ class "output" ]
                [ resultList
                , div []
                    [ text
                        ("You answered "
                            ++ String.fromInt (List.length solved)
                            ++ " questions correctly."
                        )
                    ]
                , div [] [ button [ onClick Shuffle ] [ text "Try Again" ] ]
                ]



-- DATA


triviaData : Array.Array { question : String, correct : String, distractors : List String }
triviaData =
    Array.fromList
        [ { question = "What is the capital of Japan?"
          , correct = "Tokyo"
          , distractors = [ "Osaka", "Kyoto", "Sapporo" ]
          }
        , { question = "Which planet is known as the Red Planet?"
          , correct = "Mars"
          , distractors = [ "Venus", "Jupiter", "Saturn" ]
          }
        , { question = "What is the largest ocean on Earth?"
          , correct = "Pacific Ocean"
          , distractors = [ "Atlantic Ocean", "Indian Ocean", "Arctic Ocean" ]
          }
        , { question = "Who wrote 'Hamlet'?"
          , correct = "William Shakespeare"
          , distractors = [ "Charles Dickens", "Leo Tolstoy", "Mark Twain" ]
          }
        , { question = "What is the chemical symbol for water?"
          , correct = "H2O"
          , distractors = [ "CO2", "O2", "NaCl" ]
          }
        ]
