module Common.FunctionTest exposing (..)

import Common.Function exposing (on)
import Expect
import List exposing (length, sortWith)
import Test exposing (..)


suite : Test
suite =
    describe "Common.Function"
        [ describe "on"
            [ test "((+) `on` (*2)) 2 3" <|
                always <|
                    Expect.equal 10 <|
                        on (+) ((*) 2) 2 3
            , test "sortBy (compare `on` length) [[0, 1, 2], [0, 1], [], [0]]" <|
                always <|
                    Expect.equal [ [], [ 0 ], [ 0, 1 ], [ 0, 1, 2 ] ] <|
                        sortWith (on compare length) [ [ 0, 1, 2 ], [ 0, 1 ], [], [ 0 ] ]
            ]
        ]
