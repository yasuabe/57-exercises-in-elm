module Common.MaybeEx exposing (..)

import Maybe.Extra exposing (unwrap)


mapToList : (a -> List b) -> Maybe a -> List b
mapToList =
    unwrap []


filter : (a -> Bool) -> Maybe a -> Maybe a
filter =
    Maybe.andThen << fromFilter


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
