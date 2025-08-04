module Pages.Ex57 exposing (Model, Msg(..), init, update, view)

-- # Ex57: Trivia App
--
-- - Load questions, correct answers, and wrong answers from a local file.
-- - Randomize both:
--   - Question selection.
--   - Answer order (correct + distractors).
-- - Ends on first incorrect answer or all correct.
-- - Track number of correct answers.
-- ## Constraint:
-- - Use a local file (not Redis or RDB) to store the question data.

import Array
import Common.CmdEx exposing (pureCmd)
import Html exposing (Html, button, div, input, label, li, ol, text)
import Html.Attributes exposing (checked, class, disabled, for, id, name, type_, value)
import Html.Events exposing (onClick)
import List.Extra as LE
import Maybe.Extra exposing (isNothing)
import Random exposing (Generator, generate)
import Random.List exposing (shuffle)
import String



-- MODEL


type alias Trivia =
    { question : String
    , correct : String
    , distractors : List String
    }


type alias Progress =
    ( List Trivia, List Trivia )


type alias CurrentQuestion =
    ( Trivia, List Int )


type Model
    = NotStarted
    | Questioning CurrentQuestion Progress (Maybe Int)
    | Answered CurrentQuestion Progress Int
    | Finished Progress
    | Invalid


init : Model
init =
    NotStarted


makeQuestion : Trivia -> List Int -> ( String, List ( Int, String ) )
makeQuestion { question, correct, distractors } shuffledOrd =
    (correct :: distractors)
        |> LE.zip (List.range 0 (List.length distractors))
        |> LE.zip shuffledOrd
        |> List.sortBy Tuple.first
        |> List.map Tuple.second
        |> (\l -> ( question, l ))



-- MSG


type Msg
    = Shuffle
    | ShuffleComplete (List Int)
    | Distract Trivia (List Trivia) (List Trivia)
    | DistractComplete Trivia (List Trivia) (List Trivia) (List Int)
    | OptionSelected Int
    | Submit
    | Continue



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Shuffle, _ ) ->
            ( init, generate ShuffleComplete (genRandomRange 4) )

        ( ShuffleComplete shuffled, _ ) ->
            onShuffleComplete shuffled

        ( Distract cur rest solved, _ ) ->
            onDistract model cur rest solved

        ( DistractComplete cur rest solved distractors, _ ) ->
            onDistractComplete cur rest solved distractors

        ( OptionSelected number, Questioning current progress _ ) ->
            onOptionSelected number current progress

        ( Submit, Questioning current progress (Just selection) ) ->
            ( Answered current progress selection, Cmd.none )

        ( Continue, Answered ( cur, _ ) progress selected ) ->
            onContinue progress cur selected

        ( _, _ ) ->
            ( Invalid, Cmd.none )


onShuffleComplete : List Int -> ( Model, Cmd Msg )
onShuffleComplete shuffled =
    let
        trivias =
            Array.toList triviaData
                |> LE.zip shuffled
                |> List.sortBy Tuple.first
                |> List.map Tuple.second
    in
    case trivias of
        [] ->
            ( Invalid, Cmd.none )

        cur :: rest ->
            ( init, pureCmd (Distract cur rest) [] )


onDistract : Model -> Trivia -> List Trivia -> List Trivia -> ( Model, Cmd Msg )
onDistract model cur rest solved =
    ( model, generate (DistractComplete cur rest solved) (genRandomRange 3) )


onDistractComplete : Trivia -> List Trivia -> List Trivia -> List Int -> ( Model, Cmd Msg )
onDistractComplete current rest solved distractors =
    ( Questioning ( current, distractors ) ( rest, solved ) Nothing, Cmd.none )


onOptionSelected : Int -> CurrentQuestion -> Progress -> ( Model, Cmd Msg )
onOptionSelected number current progress =
    ( Questioning current progress (Just number), Cmd.none )


onContinue : Progress -> Trivia -> Int -> ( Model, Cmd Msg )
onContinue ( sequence, solved ) cur selected =
    case ( sequence, selected ) of
        ( [], 0 ) ->
            ( Finished ( [], cur :: solved ), Cmd.none )

        ( next :: rest, 0 ) ->
            ( init, pureCmd (Distract next rest) (cur :: solved) )

        ( _, _ ) ->
            ( Finished ( cur :: sequence, solved ), Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        NotStarted ->
            viewInitialScreen

        Questioning ( currentTrivia, distractors ) ( _, solved ) selection ->
            viewShowQuestion solved currentTrivia distractors selection

        Answered ( q, distractors ) ( _, solved ) selection ->
            viewShowResult solved q distractors selection

        Finished ( [], solved ) ->
            viewEndSession solved

        Finished ( cur :: rest, solved ) ->
            viewFailedEndSession cur rest solved

        Invalid ->
            div [ class "error" ] [ text "An error occurred. Please try again." ]


viewInitialScreen : Html Msg
viewInitialScreen =
    div []
        [ div [ class "inputline" ] [ text "Click to start the trivia quiz" ]
        , div [] [ Html.button [ onClick Shuffle ] [ text "Start" ] ]
        ]


viewQuestion : List Trivia -> String -> Html Msg
viewQuestion solved question =
    div []
        [ text ("Q" ++ String.fromInt (List.length solved + 1) ++ ": " ++ question) ]


viewShowQuestion : List Trivia -> Trivia -> List Int -> Maybe Int -> Html Msg
viewShowQuestion solved trivia shuffledOrd selection =
    let
        ( question, options ) =
            makeQuestion trivia shuffledOrd

        makeListItem ( number, optionText ) =
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
    in
    div []
        [ viewQuestion solved question
        , div []
            [ ol [] (List.map makeListItem options)
            , button [ onClick Submit, disabled (isNothing selection) ]
                [ text "Submit" ]
            ]
        ]


viewShowResult : List Trivia -> Trivia -> List Int -> Int -> Html Msg
viewShowResult solved trivia shuffledDistractors selection =
    let
        ( question, options ) =
            makeQuestion trivia shuffledDistractors

        chooseClass number =
            if number == selection then
                if number == 0 then
                    "ex57__correct-answer"

                else
                    "ex57__wrong-answer"

            else if number == 0 then
                "ex57__missed-correct-answer"

            else
                "ex57__disabled-text"

        makeListItem ( number, optionText ) =
            let
                index =
                    String.fromInt number

                class_ =
                    chooseClass number
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
    in
    div []
        [ viewQuestion solved question
        , div []
            [ ol [] (List.map makeListItem options)
            , button [ onClick Continue ] [ text "Continue" ]
            ]
        ]


viewResultList : List Trivia -> Html Msg
viewResultList solved =
    List.reverse solved
        |> List.map .question
        |> List.map (\question -> li [ class "ex57__correct-question" ] [ text question ])
        |> ol []


viewFailedResultList : Trivia -> List Trivia -> List Trivia -> Html Msg
viewFailedResultList cur rest solved =
    let
        classes =
            List.repeat (List.length solved) "ex57__correct-question"
                ++ "ex57__wrong-question"
                :: List.repeat (List.length rest) "ex57__disabled-text"
    in
    List.reverse solved
        ++ (cur :: rest)
        |> List.map .question
        |> LE.zip classes
        |> List.map (\( class_, question ) -> li [ class class_ ] [ text question ])
        |> ol []


viewEndSession : List Trivia -> Html Msg
viewEndSession solved =
    div [ class "output" ]
        [ viewResultList solved
        , text "Congratulations! You answered all questions correctly."
        , button [ onClick Shuffle ] [ text "Play Again" ]
        ]


viewFailedEndSession : Trivia -> List Trivia -> List Trivia -> Html Msg
viewFailedEndSession cur rest solved =
    div [ class "output" ]
        [ viewFailedResultList cur rest solved
        , div []
            [ text
                ("You answered "
                    ++ String.fromInt (List.length solved)
                    ++ " questions correctly."
                )
            ]
        , div [] [ button [ onClick Shuffle ] [ text "Try Again" ] ]
        ]



-- UTILITY FUNCTIONS


genRandomRange : Int -> Generator (List Int)
genRandomRange upperBound =
    shuffle (List.range 0 upperBound)



-- DATA


triviaData : Array.Array Trivia
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
