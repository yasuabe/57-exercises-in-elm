module Common.CmdEx exposing (..)

import Task


withNone : a -> ( a, Cmd msg )
withNone model =
    ( model, Cmd.none )


pureCmd : (a -> msg) -> a -> Cmd msg
pureCmd msgFunc =
    Task.succeed >> Task.perform msgFunc
