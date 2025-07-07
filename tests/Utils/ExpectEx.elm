module Utils.ExpectEx exposing (..)

import Expect exposing (..)
import Test exposing (..)


equateFloats : Float -> Float -> Expectation
equateFloats a b =
    Expect.within (Absolute 0.000000001) a b
