module Pages.Ex04Test exposing (..)

import Expect
import Pages.Ex04 as Ex04
import Test exposing (..)


suite : Test
suite =
    describe "Ex04 Module"
        [ describe "update"
            [ test "VerbChanged should update verb field" <|
                always
                    (Expect.all
                        [ \f -> f (Ex04.VerbChanged "v001") <| Just "v001"
                        , \f -> f (Ex04.VerbChanged "") <| Nothing
                        ]
                        (testUpdate .verb)
                    )
            , test "NounChanged should update noun field" <|
                always
                    (Expect.all
                        [ \f -> f (Ex04.NounChanged "n001") <| Just "n001"
                        , \f -> f (Ex04.NounChanged "") <| Nothing
                        ]
                        (testUpdate .noun)
                    )
            , test "AdjectiveChanged should update adjective field" <|
                always
                    (Expect.all
                        [ \f -> f (Ex04.AdjectiveChanged "a001") <| Just "a001"
                        , \f -> f (Ex04.AdjectiveChanged "") <| Nothing
                        ]
                        (testUpdate .adjective)
                    )
            , test "AdverbChanged should update adverb field" <|
                always
                    (Expect.all
                        [ \f -> f (Ex04.AdverbChanged "a001") <| Just "a001"
                        , \f -> f (Ex04.AdverbChanged "") <| Nothing
                        ]
                        (testUpdate .adverb)
                    )
            , test "Submit with non-empty quote and author should update output" <|
                \_ -> testSubmit "n1" "v1" "adj1" "adv1" <| Just "Do you v1 your adj1 n1 adv1?\nThat's hilarious!"
            , test "Submit with empty hole should empty output" <|
                \_ -> testSubmit "" "v1" "adj1" "adv1" Nothing
            ]
        ]


testUpdate : (Ex04.Model -> Maybe String) -> Ex04.Msg -> Maybe String -> Expect.Expectation
testUpdate extractor msg expect =
    Ex04.init
        |> Ex04.update msg
        |> Tuple.first
        |> extractor
        |> Expect.equal expect


testSubmit : String -> String -> String -> String -> Maybe String -> Expect.Expectation
testSubmit n v adj adv expect =
    let
        output =
            Ex04.init
                |> Ex04.update (Ex04.NounChanged n)
                |> Tuple.first
                |> Ex04.update (Ex04.VerbChanged v)
                |> Tuple.first
                |> Ex04.update (Ex04.AdjectiveChanged adj)
                |> Tuple.first
                |> Ex04.update (Ex04.AdverbChanged adv)
                |> Tuple.first
                |> Ex04.makeOutput
    in
    Expect.equal output expect
