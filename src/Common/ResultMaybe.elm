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
    let
        trimmed =
            String.trim str
    in
    if String.isEmpty trimmed then
        Ok Nothing

    else
        case converter trimmed of
            Just x ->
                Ok <| Just x

            Nothing ->
                Err errMsg


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

withDefault : a -> ResultMaybe e a -> a
withDefault default result =
    case result of
        Ok (Just x) ->
            x

        _ ->
            default