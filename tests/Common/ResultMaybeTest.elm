module Common.ResultMaybeTest exposing (..)

import Common.ResultMaybe exposing (collectErrors, collectErrors2, map, map2, map3, convertStringToRM)
import Expect exposing (..)
import Test exposing (..)
import Common.ResultMaybe exposing (parseString)


suite : Test
suite =
    describe "Common.ResultMaybe"
        [ describe "collect errors"
            [ test "empty list yields empty lists" <|
                \_ ->
                    collectErrors []
                        |> equalLists []
            , test "collect error values from given List of ResultMaybe" <|
                \_ ->
                    collectErrors [ Err 1, Ok (Just "a"), Err 2, Ok (Just "b") ]
                        |> equalLists [ 1, 2 ]
            ]
        , describe "collectErrors2"
            [ test "collect errors from given two `ResultMaybe`s" <|
                \_ -> equalLists [ 1, 2 ] <| collectErrors2 (Err 1) (Err 2)
            , test "collect error from the first `ResultMaybe`" <|
                \_ -> equalLists [ 1 ] <| collectErrors2 (Err 1) (Ok Nothing)
            , test "collect error from the second `ResultMaybe`" <|
                \_ -> equalLists [ "2" ] <| collectErrors2 (Ok Nothing) (Err "2")
            , test "return an empty list if both `ResultMaybe`s are not Err" <|
                \_ -> equalLists [] <| collectErrors2 (Ok Nothing) (Ok <| Just "2")
            ]
        , describe "map"
            [ test "apply function to x in (Ok Just x)" <|
                \_ ->
                    map String.fromFloat (Ok <| Just 3.14) |> equal (Ok <| Just "3.14")
            , test "Nothing is preserved" <|
                \_ ->
                    map String.fromFloat (Ok Nothing) |> equal (Ok Nothing)
            , test "Err e is preserved" <|
                \_ ->
                    map String.fromFloat (Err "error") |> equal (Err "error")
            ]
        , describe "map2"
            [ test "apply function f to (Ok Just x, Ok Just y)" <|
                \_ ->
                    map2 (+) (Ok <| Just 3.14) (Ok <| Just 2.71) |> equal (Ok <| Just 5.85)
            ]
        , describe "map3"
            [ test "apply function to (Ok Just x, Ok Just y, Ok Just z)" <|
                \_ ->
                    map3
                        (\a b c -> ( a, b, c ))
                        (Ok <| Just 3.14)
                        (Ok <| Just 2.71)
                        (Ok <| Just 1.41)
                        |> equal (Ok <| Just ( 3.14, 2.71, 1.41 ))
            ]
        , describe "withDefault"
            [ test "return default value if the result is Err" <|
                \_ ->
                    Common.ResultMaybe.withDefault "default" (Err "error")
                        |> equal "default"
            , test "return value if the result is Ok Just value" <|
                \_ ->
                    Common.ResultMaybe.withDefault "default" (Ok <| Just "value")
                        |> equal "value"
            , test "return default value if the result is Ok Nothing" <|
                \_ ->
                    Common.ResultMaybe.withDefault "default" (Ok Nothing)
                        |> equal "default"
            ]
            , describe "convertStringToRM"
                [ test "convert string to ResultMaybe" <|
                    \_ ->
                        convertStringToRM String.toFloat (always "error") " 3.14"
                            |> equal (Ok <| Just 3.14)
                , test "return Err if conversion fails" <|
                    \_ ->
                        convertStringToRM String.toFloat (\s -> "NG:" ++ s) "invalid"
                            |> equal (Err "NG:invalid")
                ]
            , describe "parseString"
                [ test "convert string to ResultMaybe" <|
                    \_ ->
                        parseString String.toFloat "error" " 3.14"
                            |> equal (Ok <| Just 3.14)
                , test "return Err if conversion fails" <|
                    \_ ->
                        parseString String.toFloat "NG" "invalid"
                            |> equal (Err "NG")
                ]
        ]
