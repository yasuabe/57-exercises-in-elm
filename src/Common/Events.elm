module Common.Events exposing (submitOnEnter)

import Json.Decode as Decode


submitOnEnter : msg -> Decode.Decoder msg
submitOnEnter submit =
    Decode.field "key" Decode.string
        |> Decode.andThen (keyDecoder submit)


keyDecoder : msg -> String -> Decode.Decoder msg
keyDecoder submit key =
    if key == "Enter" then
        Decode.succeed submit

    else
        Decode.fail "Not Enter key"
