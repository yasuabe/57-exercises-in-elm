module Common.ResultEx exposing (..)


either : (a -> b) -> (e -> b) -> Result e a -> b
either okFn errFn x =
    case x of
        Ok v ->
            okFn v

        Err e ->
            errFn e


fromEither : Result a a -> a
fromEither =
    either identity identity


mapEither : (a -> c) -> (e -> b) -> Result e a -> Result b c
mapEither okFn errFn x =
    case x of
        Ok v ->
            Ok (okFn v)

        Err e ->
            Err (errFn e)
