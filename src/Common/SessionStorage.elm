port module Common.SessionStorage exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


port setItem : (String, Encode.Value) -> Cmd msg


port getItem : String -> Cmd msg


port itemReceived : (Decode.Value -> msg) -> Sub msg
