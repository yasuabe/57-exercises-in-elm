module Pages.Ex33Test exposing (..)

import Expect
import Pages.Ex33 as Ex33
import Test exposing (..)
import Tuple exposing (first)


suite : Test
suite =
    describe "Ex33 Module"
        [ describe "update"
            [ test "GotSelectedIndex should update the output" <|
                \_ ->
                    let
                        test index expected model =
                            model
                                |> Ex33.update (Ex33.GotSelectedIndex index)
                                |> first
                                |> expectOutputContains expected
                    in
                    Expect.all
                        [ test 0 "Yes"
                        , test 1 "No"
                        , test 2 "Maybe"
                        , test 3 "Ask again later"
                        ]
                        Ex33.init
            ]
        ]


expectOutputContains : String -> Ex33.Model -> Expect.Expectation
expectOutputContains expected model =
    case model.output of
        Ok (Just message) ->
            Expect.equal True <| String.contains expected message

        _ ->
            Expect.fail "unreachable: output should be Ok with a message"
