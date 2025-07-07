module Common.Math exposing (..)


roundToDecimals : Float -> Int -> Float
roundToDecimals x p =
    toFloat (round (x * toFloat (10 ^ p))) / toFloat (10 ^ p)
