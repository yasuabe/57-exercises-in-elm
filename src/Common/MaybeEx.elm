module Common.MaybeEx exposing (..)


mapToList : (a -> List b) -> Maybe a -> List b
mapToList f maybeValue =
    case maybeValue of
        Just value ->
            f value     
        Nothing ->
            []