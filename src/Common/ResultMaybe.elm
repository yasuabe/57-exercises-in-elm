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
