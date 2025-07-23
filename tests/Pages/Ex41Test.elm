module Pages.Ex41Test exposing (..)

import Expect
import Pages.Ex41 as Ex41
import Test exposing (..)
import Tuple exposing (first)


suite : Test
suite =
    describe "Ex41 Module"
        [ describe "update"
            [ test "should sort the names and update the model" <|
                \_ ->
                    let
                        source =
                            String.join "\n" <|
                                [ "Bob,25"
                                , "Charlie"
                                , "Alice"
                                ]

                        newModel =
                            Ex41.init
                                |> Ex41.update (Ex41.SortNames source)
                                |> first
                    in
                    Expect.equal newModel.sorted <|
                        Just <|
                            String.join "\n" <|
                                [ "Total: 3 Names"
                                , "-----------------"
                                , "Alice"
                                , "Bob,25"
                                , "Charlie"
                                ]
            ]
        ]
