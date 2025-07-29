module Common.HttpEx exposing (..)

import Http exposing (Error)


errorToString : Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error occurred"

        Http.BadStatus status ->
            "Bad status code: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad body: " ++ body
