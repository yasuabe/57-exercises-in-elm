module Common.MaybeEx exposing (..)


maybe : b -> (a -> b) -> Maybe a -> b
maybe n f maybeValue =
    case maybeValue of
        Just value ->
            f value

        Nothing ->
            n


filter : (a -> Bool) -> Maybe a -> Maybe a
filter =
    Maybe.andThen << fromFilter


mapToList : (a -> List b) -> Maybe a -> List b
mapToList f maybeValue =
    maybe [] f maybeValue


fromFilter : (a -> Bool) -> a -> Maybe a
fromFilter predicate value =
    if predicate value then
        Just value

    else
        Nothing


-- Maybe String

toMaybe : String -> Maybe String
toMaybe =
    String.trim >> fromFilter (String.isEmpty >> not)


fromMaybe : Maybe String -> String
fromMaybe =
    Maybe.withDefault ""

