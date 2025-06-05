module Pages.Ex04Test exposing (..)

import Expect
import Pages.Ex04 as Ex04
import Test exposing (..)


suite : Test
suite =
    describe "Ex04 Module"
        [ describe "init"
            [ test "should initialize with empty noun, verb, adjective, adverb and message" <|
                \_ ->
                    let
                        isEmpty f =
                            \m -> Expect.equal "" (f m)
                    in
                    Expect.all
                        [ isEmpty (\m -> m.noun)
                        , isEmpty (\m -> m.verb)
                        , isEmpty (\m -> m.adjective)
                        , isEmpty (\m -> m.adverb)
                        ]
                        Ex04.init
            ]
        , describe "update"
            [ test "VerbChanged should update verb field" <|
                \_ -> testUpdate (Ex04.VerbChanged "v001") (\m -> m.verb) "v001"
            , test "NounChanged should update noun field" <|
                \_ -> testUpdate (Ex04.NounChanged "a001") (\m -> m.noun) "a001"
            , test "AdjectiveChanged should update adjective field" <|
                \_ -> testUpdate (Ex04.AdjectiveChanged "a001") (\m -> m.adjective) "a001"
            , test "AdverbChanged should update adverb field" <|
                \_ -> testUpdate (Ex04.AdverbChanged "a001") (\m -> m.adverb) "a001"
            , test "Submit with non-empty quote and author should update output" <|
                \_ -> testSubmit "n1" "v1" "adj1" "adv1" "Do you v1 your adj1 n1 adv1? That's hilarious!"
            , test "Submit with empty hole should empty output" <|
                \_ -> testSubmit "" "v1" "adj1" "adv1" ""
            ]
        ]


testUpdate : Ex04.Msg -> (Ex04.Model -> String) -> String -> Expect.Expectation
testUpdate msg extractor expect =
    let
        ( newModel, _ ) =
            Ex04.update msg <| Ex04.init
    in
    Expect.equal expect <| extractor newModel


testSubmit : String -> String -> String -> String -> String -> Expect.Expectation
testSubmit n v adj adv expect =
    let
        model =
            { noun = n, verb = v, adjective = adj, adverb = adv, message = "---" }

        ( newModel, _ ) =
            Ex04.update Ex04.Submit model
    in
    Expect.equal expect newModel.message
