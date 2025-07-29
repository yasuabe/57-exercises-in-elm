module Pages.Ex48Test exposing (..)

import Pages.Ex48 as Ex48
import Test exposing (..)
import Test.Html.Selector exposing (..)
import Utils.ExpectEx exposing (equateFloats)


suite : Test
suite =
    describe "Ex48 Module"
        [ describe "Kelvin to fahrenheit"
            [ test "306.73 -> 92.444" <|
                \_ ->
                    Ex48.kelvinToFahrenheit 306.73 |> equateFloats 92.4
            ]
        ]
