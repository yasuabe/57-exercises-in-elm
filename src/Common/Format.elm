module Common.Format exposing (..)


padZeros : Int -> Int -> String
padZeros digits n =
    let
        loop : Int -> Int -> List Char -> String
        loop d m chars =
            if d < 1 && m <= 0 then
                String.fromList chars

            else
                loop (d - 1) (m // 10) (Char.fromCode (modBy 10 m + Char.toCode '0') :: chars)
    in
    loop digits n []


toTwoDigits : Int -> String
toTwoDigits =
    padZeros 2 >> String.right 2


toTimeString : Int -> Int -> Int -> String
toTimeString hour minute second =
    [ hour, minute, second ]
        |> List.map toTwoDigits
        |> String.join ":"
