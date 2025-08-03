module Common.Events exposing (onEnter, onEnter2, withNone)

import Html exposing (Attribute)
import Html.Events exposing (on, targetValue)
import Json.Decode as Decode exposing (andThen, fail, field, string, succeed)


enterKeyDecoder : msg -> Decode.Decoder msg
enterKeyDecoder msg =
    let
        decoder key =
            if key == "Enter" then
                succeed msg

            else
                fail "Not the Enter key"
    in
    field "key" string |> andThen decoder



-- TODO: remove after all usages are removed


onEnter : msg -> Attribute msg
onEnter msg =
    on "keydown" <| enterKeyDecoder msg


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


onEnter2 : (String -> msg) -> Attribute msg
onEnter2 msg =
    on "keydown" <| enterKeyDecoder2 msg


withNone : a -> ( a, Cmd msg )
withNone model =
    ( model, Cmd.none )