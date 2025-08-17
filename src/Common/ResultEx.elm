module Common.ResultEx exposing (..)

import Result.Extra exposing (unpack)


either : (a -> b) -> (e -> b) -> Result e a -> b
either okFn errFn =
    unpack errFn okFn
