module Pages.Ex46Test exposing (..)

import Expect
import Pages.Ex46 as Ex46 exposing (..)
import Test exposing (..)
import Tuple exposing (first)


suite : Test
suite =
    describe "Ex46 Module"
        [ describe "countWords"
            [ test "should sort the names and update the model" <|
                \_ ->
                    let
                        source =
                            String.join "\n" <|
                                [ "badger badger badger mushroom mushroom snake badger badger"
                                , "badger badger badger bear"
                                ]
                    in
                    Ex46.init
                        |> applyMessage (Ex46.FileLoaded source)
                        |> countWords
                        |> Expect.equal
                            [ ( "badger", 8 ), ( "mushroom", 2 ), ( "bear", 1 ), ( "snake", 1 ) ]
            ]
        ]


applyMessage : Msg -> Model -> Model
applyMessage msg =
    Ex46.update msg >> first
