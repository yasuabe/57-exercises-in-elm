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


type alias Model =
    { sequence : List Int
    , currentQuestion : Maybe ( Int, List Int )
    , selection : Maybe Int
    , correct : Maybe Bool
    , finished : Bool
    }


init : Model
init =
    { sequence = []
    , currentQuestion = Nothing
    , selection = Nothing
    , correct = Nothing
    , finished = False
    }



-- MSG


type Msg
    = Shuffle
    | ShuffleComplete (List Int)
    | Distract
    | DistractComplete (List Int)
    | OptionSelected Int
    | Submit
    | Continue



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shuffle ->
            ( init, generate ShuffleComplete (shuffle <| List.range 0 4) )

        ShuffleComplete shuffled ->
            ( { model | sequence = shuffled }, Task.perform (always Distract) (Task.succeed ()) )

        Distract ->
            case model.sequence of
                [] ->
                    ( { model | currentQuestion = Nothing }, Cmd.none )

                _ ->
                    ( model, generate DistractComplete (shuffle (List.range 0 3)) )

        DistractComplete distractors ->
            case model.sequence of
                q :: qs ->
                    ( { model | currentQuestion = Just ( q, distractors ), sequence = qs }, Cmd.none )

                [] ->
                    ( model, Cmd.none )

        OptionSelected number ->
            ( { model | selection = Just number }, Cmd.none )

        Submit ->
            ( { model | correct = Maybe.map ((==) 0) model.selection }, Cmd.none )

        Continue ->
            case ( model.sequence, model.correct ) of
                ( _ :: _, Just True ) ->
                    ( { model | selection = Nothing, correct = Nothing }
                    , Task.perform (always Distract) (Task.succeed ())
                    )

                ( _, _ ) ->
                    ( { model | finished = True }, Cmd.none )



-- Here you would handle the shuffled list if needed
-- VIEW


view : Model -> Html Msg
view model =
    case ( model.currentQuestion, model.correct, model.finished ) of
        ( Nothing, _, False ) ->
            div []
                [ div []
                    [ text "Click to shuffle the sequence"
                    , Html.button [ onClick Shuffle ] [ text "Play" ]
                    ]
                ]

        ( Just ( q, distractors ), Nothing, False ) ->
            viewShowQuestion ( q, distractors, model.selection )

        ( Just ( q, distractors ), Just _, False ) ->
            viewShowResult ( q, distractors, model.selection )

        ( _, _, True ) ->
            viewEndSession model


viewShowQuestion : ( Int, List Int, Maybe Int ) -> Html Msg
viewShowQuestion ( questionNumber, shuffledDistractors, selection ) =
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


viewShowResult : ( Int, List Int, Maybe Int ) -> Html Msg
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
                                let
                                    index =
                                        String.fromInt number

                                    class_ =
                                        if Just number == selection then
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
                                        , checked (Just number == selection)
                                        , onClick (OptionSelected number)
                                        ]
                                        []
                                    , label [ for index ] [ text optionText ]
                                    ]
                            )
                            options
                        )
                    , button [ onClick Continue, disabled (isNothing selection) ]
                        [ text "Continue" ]
                    ]
                ]


viewEndSession : Model -> Html Msg
viewEndSession model =
    case model.correct of
        Just True ->
            div []
                [ text "Congratulations! You answered all questions correctly."
                , button [ onClick Shuffle ]
                    [ text "Play Again" ]
                ]

        Just False ->
            let
                correctCount =
                    Array.length triviaData - List.length model.sequence - 1
            in
            div []
                [ text ("You answered " ++ String.fromInt correctCount ++ " questions correctly. Better luck next time!")
                , button [ onClick Shuffle ]
                    [ text "Try Again" ]
                ]

        Nothing ->
            div [] [ text "No trivia data available." ]



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
