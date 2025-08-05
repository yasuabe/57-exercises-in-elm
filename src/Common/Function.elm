module Common.Function exposing (..)


on : (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y =
    f (g x) (g y)


choose : Bool -> a -> a -> a
choose condition a b =
    if condition then
        a

    else
        b
