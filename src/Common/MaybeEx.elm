module Common.MaybeEx exposing (..)


filter : (a -> Bool) -> Maybe a -> Maybe a
filter predicate maybeValue =
    Maybe.andThen
        (\value ->
            if predicate value then
                Just value

            else
                Nothing
        )
        maybeValue


mapToList : (a -> List b) -> Maybe a -> List b
mapToList f maybeValue =
    case maybeValue of
        Just value ->
            f value

        Nothing ->
            []
