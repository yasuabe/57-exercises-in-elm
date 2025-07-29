module Common.Math exposing (..)


roundToDecimals : Int -> Float -> Float
roundToDecimals p x =
    toFloat (round (x * toFloat (10 ^ p))) / toFloat (10 ^ p)
