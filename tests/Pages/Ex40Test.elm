module Pages.Ex40Test exposing (..)

import Expect
import Pages.Ex40 as Ex40
import Test exposing (..)
import Tuple exposing (first)


suite : Test
suite =
    describe "Ex40 Module"
        [ describe "filterEmployees"
            [ test "should filter by name" <|
                \_ ->
                    Ex40.init
                        |> updateModel (Ex40.NameChanged "Jac")
                        |> Ex40.filterEmployees
                        |> List.map .firstName
                        |> Expect.equalLists [ "Jake", "Jacquelyn" ]
            , test "should filter by position" <|
                \_ ->
                    Ex40.init
                        |> updateModel (Ex40.PositionChanged "Manager")
                        |> Ex40.filterEmployees
                        |> List.map .firstName
                        |> Expect.equalLists [ "John" ]
            , test "should filter by name and position" <|
                \_ ->
                    Ex40.init
                        |> updateModel (Ex40.NameChanged "a")
                        |> updateModel (Ex40.PositionChanged "DBA")
                        |> Ex40.filterEmployees
                        |> List.map .firstName
                        |> Expect.equalLists [ "Jacquelyn" ]
            , test "should not use 'none' as position filter" <|
                \_ ->
                    Ex40.init
                        |> updateModel (Ex40.NameChanged "el")
                        |> updateModel (Ex40.PositionChanged "none")
                        |> Ex40.filterEmployees
                        |> List.map .firstName
                        |> Expect.equalLists [ "Michaela", "Jacquelyn" ]
            ]
        ]


updateModel : Ex40.Msg -> Ex40.Model -> Ex40.Model
updateModel msg model =
    Ex40.update msg model |> first
