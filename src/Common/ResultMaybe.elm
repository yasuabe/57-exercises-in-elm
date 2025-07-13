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

parseString: (String -> Maybe a)-> String -> String -> ResultMaybe String a
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

parseStringToFloat: String -> String -> ResultMaybe String Float
parseStringToFloat errMsg str = parseString String.toFloat errMsg str