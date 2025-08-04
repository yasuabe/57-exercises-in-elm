module Common.Events exposing (onBlur, onEnter)

import Html exposing (Attribute)
import Html.Events exposing (on, targetValue)
import Json.Decode as Decode exposing (andThen, fail, field, string)


enterKeyDecoder2 : (String -> msg) -> Decode.Decoder msg
enterKeyDecoder2 msg =
    let
        decoder key =
            if key == "Enter" then
                Decode.map msg targetValue

            else
                fail "Not the Enter key"
    in
    field "key" string |> andThen decoder


onEnter : (String -> msg) -> Attribute msg
onEnter msg =
    on "keydown" <| enterKeyDecoder2 msg


onBlur : (String -> msg) -> Attribute msg
onBlur msg =
    on "blur" (Decode.map msg targetValue)
