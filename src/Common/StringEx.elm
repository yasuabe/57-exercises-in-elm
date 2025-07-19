module Common.StringEx exposing (..)


quote : Char -> String -> String
quote c str =
    let
        q =
            String.fromChar c
    in
    q ++ str ++ q


sort : String -> String
sort str =
    String.toList str
        |> List.sort
        |> String.fromList
