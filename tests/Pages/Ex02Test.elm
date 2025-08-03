module Pages.Ex02Test exposing (..)

import Expect
import Pages.Ex02 as Ex02
import Test exposing (..)


suite : Test
suite =
    describe "Ex02 Module"
        [ describe "update"
            [ test "Submit with non-empty input should report character count" <|
                always
                    (Ex02.init
                        |> Ex02.update (Ex02.Submit "Homer")
                        |> Tuple.first
                        |> Ex02.makeOutput
                        |> Expect.equal (Just "Homer has 5 characters.")
                    )
            ]
        ]
