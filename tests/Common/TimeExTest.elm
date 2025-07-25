module Common.TimeExTest exposing (..)

import Common.TimeEx as SUT
import Expect exposing (..)
import Test exposing (..)
import Time as T exposing (Month(..), utc)


suite : Test
suite =
    describe "Common.TimeEx"
        [ describe "decompose"
            [ test "should extract data/time elements" <|
                \_ ->
                    let
                        ((y, m, d), (h, mm, s)) = SUT.decompose utc <| T.millisToPosix 0
                    in
                    ((y, m, d), (h, mm, s))
                        |> equal ((1970, Jan, 1), (0, 0, 0))
            ]
        , describe "toEnglishMonth"
            [ test "should convert month to English name" <|
                \_ ->
                    Expect.all
                        [ \_ -> SUT.toEnglishMonth Jan |> equal "January"
                        , \_ -> SUT.toEnglishMonth Feb |> equal "February"
                        , \_ -> SUT.toEnglishMonth Mar |> equal "March"
                        , \_ -> SUT.toEnglishMonth Apr |> equal "April"
                        , \_ -> SUT.toEnglishMonth May |> equal "May"
                        , \_ -> SUT.toEnglishMonth Jun |> equal "June"
                        , \_ -> SUT.toEnglishMonth Jul |> equal "July"
                        , \_ -> SUT.toEnglishMonth Aug |> equal "August"
                        , \_ -> SUT.toEnglishMonth Sep |> equal "September"
                        , \_ -> SUT.toEnglishMonth Oct |> equal "October"
                        , \_ -> SUT.toEnglishMonth Nov |> equal "November"
                        , \_ -> SUT.toEnglishMonth Dec |> equal "December"
                        ] ()
            ]
        ]