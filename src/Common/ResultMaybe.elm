module Common.ResultMaybe exposing (..)

import List exposing (filterMap)


type alias ResultMaybe e a =
    Result e (Maybe a)


collectErrors : List (ResultMaybe e a) -> List e
collectErrors results =
    filterMap
        (\e ->
            case e of
                Err m ->
                    Just m

                _ ->
                    Nothing
        )
        results


collectErrors2 : ResultMaybe e a -> ResultMaybe e b -> List e
collectErrors2 rm1 rm2 =
    case ( rm1, rm2 ) of
        ( Err e1, Err e2 ) ->
            [ e1, e2 ]

        ( Err e1, _ ) ->
            [ e1 ]

        ( _, Err e2 ) ->
            [ e2 ]

        _ ->
            []


parseString : (String -> Maybe a) -> String -> String -> ResultMaybe String a
parseString converter errMsg str =
    convertStringToRM converter (always errMsg) str


parseStringToFloat : String -> String -> ResultMaybe String Float
parseStringToFloat =
    parseString String.toFloat


map : (a -> b) -> ResultMaybe e a -> ResultMaybe e b
map f result =
    case result of
        Ok (Just x) ->
            Ok (Just (f x))

        Ok Nothing ->
            Ok Nothing

        Err e ->
            Err e


map2 : (x -> y -> r) -> ResultMaybe e x -> ResultMaybe e y -> ResultMaybe e r
map2 f x_ y_ =
    case ( x_, y_ ) of
        ( Err e, _ ) ->
            Err e

        ( _, Err e ) ->
            Err e

        ( Ok Nothing, _ ) ->
            Ok Nothing

        ( _, Ok Nothing ) ->
            Ok Nothing

        ( Ok (Just x), Ok (Just y) ) ->
            Ok (Just (f x y))


map3 :
    (x -> y -> z -> r)
    -> ResultMaybe e x
    -> ResultMaybe e y
    -> ResultMaybe e z
    -> ResultMaybe e r
map3 f x_ y_ z_ =
    map2 (\b c -> b c) (map2 f x_ y_) z_


map4 :
    (x -> y -> z -> w -> r)
    -> ResultMaybe e x
    -> ResultMaybe e y
    -> ResultMaybe e z
    -> ResultMaybe e w
    -> ResultMaybe e r
map4 f x_ y_ z_ w_ =
    map2 (\b c -> b c) (map3 f x_ y_ z_) w_


withDefault : a -> ResultMaybe e a -> a
withDefault default result =
    case result of
        Ok (Just x) ->
            x

        _ ->
            default


convertToRM : (from -> Maybe a) -> (from -> Bool) -> (from -> String) -> from -> ResultMaybe String a
convertToRM convert isEmpty toErrMsg from =
    case convert from of
        Just value ->
            Ok (Just value)

        Nothing ->
            if isEmpty from then
                Ok Nothing

            else
                Err (toErrMsg from)


convertStringToRM : (String -> Maybe a) -> (String -> String) -> String -> ResultMaybe String a
convertStringToRM convert strToErrMsg str =
    convertToRM convert String.isEmpty strToErrMsg <| String.trim str


convertInputToField : (String -> Maybe a) -> String -> ResultMaybe String a
convertInputToField convert =
    convertToRM convert String.isEmpty String.trim


convertInputToFloatField : String -> ResultMaybe String Float
convertInputToFloatField =
    convertInputToField (String.trim >> String.toFloat)


convertInputToIntField : String -> ResultMaybe String Int
convertInputToIntField =
    convertInputToField (String.trim >> String.toInt)
