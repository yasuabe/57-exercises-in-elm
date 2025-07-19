module Pages.Ex24Test exposing (..)

import Common.ResultMaybe exposing (ResultMaybe)
import Expect
import Pages.Ex24 as Ex24 exposing (Model, Msg(..), update)
import Test exposing (..)
import Utils.ExpectEx exposing (checkStringContains)


suite : Test
suite =
    describe "Ex24 Module"
        [ describe "update"
            [ test "recognize anagram" <|
                \_ ->
                    Expect.all
                        [ validateUpdate "note" "tone" "are anagrams"
                        , validateUpdate "note" " eNtO " "are anagrams"
                        ]
                        Ex24.init
            , test "recognize non-anagram" <|
                \_ ->
                    Expect.all
                        [ validateUpdate "note" "notc" "are not anagrams"
                        , validateUpdate "note" "n0te" "are not anagrams"
                        ]
                        Ex24.init
            , test "returns nothing if one input is missing" <|
                \_ ->
                    Expect.all
                        [ Expect.equal (Ok Nothing) << applyEvents [ FirstChanged "a" ]
                        , Expect.equal (Ok Nothing) << applyEvents [ SecondChanged "b" ]
                        ]
                        Ex24.init
            , test "returns error for different length intputs" <|
                \_ ->
                    Expect.all
                        [ expectInputError "note" "ton"
                        , expectInputError "note" "tonee"
                        ]
                        Ex24.init
            ]
        ]


validateUpdate : String -> String -> String -> Model -> Expect.Expectation
validateUpdate first second expected model =
    let
        newOutput =
            applyUpdates first second model
    in
    case newOutput of
        Ok (Just actual) ->
            checkStringContains expected actual

        _ ->
            Expect.fail "Unexpected output: expected Ok with a message"


expectInputError : String -> String -> Model -> Expect.Expectation
expectInputError first second model =
    let
        newOutput =
            applyUpdates first second model
    in
    case newOutput of
        Err (actual :: []) ->
            checkStringContains "must have the same length" actual

        _ ->
            Expect.fail "Unexpected output: expected Ok with a message"


applyUpdates : String -> String -> Model -> ResultMaybe (List String) String
applyUpdates first second =
    applyEvents [ FirstChanged first, SecondChanged second ]


applyEvents : List Msg -> Model -> ResultMaybe (List String) String
applyEvents messages model =
    let
        update msg m =
            Ex24.update msg m |> Tuple.first
    in
    (List.foldl update model messages).output
