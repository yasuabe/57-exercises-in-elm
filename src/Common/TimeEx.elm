module Common.TimeEx exposing (..)

import Time as T exposing (Month(..), Posix, Zone)


decompose : Zone -> Posix -> ( ( Int, Month, Int ), ( Int, Int, Int ) )
decompose zone posix =
    let
        f fn =
            fn zone posix

        year =
            f T.toYear

        month =
            f T.toMonth

        day =
            f T.toDay

        hour =
            f T.toHour

        minute =
            f T.toMinute

        second =
            f T.toSecond
    in
    ( ( year, month, day ), ( hour, minute, second ) )


toEnglishMonth : Month -> String
toEnglishMonth month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"
