module Pages.Ex03Test exposing (..)

import Expect
import Pages.Ex03 as Ex03
import Test exposing (..)


suite : Test
suite =
    describe "Ex03 Module"
        [ describe "update"
            [ test "Submit with non-empty quote and author should update output" <|
                \_ -> testSubmit "q002" "a002" (Just "a002 says, \"q002\"")
            , test "Submit with empty quote should set output empty" <|
                \_ -> testSubmit "" "a003" Nothing
            , test "Submit with empty author should set output empty" <|
                \_ -> testSubmit "q004" "" Nothing
            ]
        ]


testSubmit : String -> String -> Maybe String -> Expect.Expectation
testSubmit quote author expect =
    let
        maybeMessage = Ex03.init
            |> Ex03.update (Ex03.QuoteChanged quote)
            |> Tuple.first
            |> Ex03.update (Ex03.AuthorChanged author)
            |> Tuple.first
            |> Ex03.makeOutput
    in
    Expect.equal expect maybeMessage
