module Utils exposing (suite, suite1)

import Expect
import Main exposing (..)
import Test exposing (describe, only, test)


suite =
    describe "split"
        [ test "one split point" <|
            \_ ->
                let
                    x =
                        [ 0, 1, 2, 3, 4 ]
                in
                Expect.equal [ [ 0 ], [ 1, 2, 3, 4 ] ] (split [ 1 ] x)
        , test "many split points" <|
            \_ ->
                let
                    x =
                        [ 0, 1, 2, 3, 4, 5, 6 ]
                in
                Expect.equal [ [ 0 ], [ 1 ], [ 2, 3, 4 ], [ 5, 6 ] ] (split [ 1, 2, 5 ] x)
        , test "overlapping split points" <|
            \_ ->
                let
                    x =
                        [ 0, 1, 2, 3, 4, 5, 6 ]
                in
                Expect.equal [ [ 0 ], [ 1 ], [ 2, 3, 4 ], [ 5, 6 ] ] (split [ 1, 2, 2, 5 ] x)
        , test "unsorted split points" <|
            \_ ->
                let
                    x =
                        [ 0, 1, 2, 3, 4, 5, 6 ]
                in
                Expect.equal [ [ 0 ], [ 1 ], [ 2, 3, 4, 5 ], [ 6 ] ] (split [ 2, 6, 2, 1 ] x)
        , test "split points at list bounds" <|
            \_ ->
                let
                    x =
                        [ 0, 1, 2, 3, 4, 5, 6 ]
                in
                Expect.equal [ [], [ 0, 1 ], [ 2, 3, 4 ], [ 5, 6 ], [] ] (split [ 0, 2, 5, 8 ] x)
        , test "split points out list bounds" <|
            \_ ->
                let
                    x =
                        [ 0, 1, 2, 3, 4, 5, 6 ]
                in
                Expect.equal [ [], [ 0, 1, 2 ], [ 3, 4 ], [ 5, 6 ], [] ] (split [ -2, 3, 5, 15 ] x)
        , test "empty list" <|
            \_ ->
                let
                    x =
                        []
                in
                Expect.equal [ [], [], [], [], [] ] (split [ -2, 3, 5, 15 ] x)
        , test "empty points list" <|
            \_ ->
                let
                    x =
                        [ 0, 1, 2, 3, 4, 5, 6 ]
                in
                Expect.equal [ [ 0, 1, 2, 3, 4, 5, 6 ] ] (split [] x)
        ]


suite1 =
    describe "move"
        [ test "move element to target position" <|
            \_ ->
                let
                    x =
                        [ 0, 1, 2, 3 ]
                in
                Expect.equal [ 0, 2, 1, 3 ] (move 1 1 3 x)

        , test "move element with length more than 1 to target position" <|
            \_ ->
                let
                    x =
                        [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
                in
                Expect.equal [ 0, 4, 1, 2, 3, 5, 6, 7, 8, 9 ] (move 1 3 5 x)

        , test "return unchanged list if target position is inside the element" <|
            \_ ->
                let
                    x =
                        [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
                in
                Expect.equal [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ] (move 1 4 2 x)

        , test "return unchanged list if target is out of list bounds 1" <|
            \_ ->
                let
                    x =
                        [ 0, 1, 2]
                in
                Expect.equal [ 0, 1, 2 ] (move 1 1 4 x)
        ,  test "return unchanged list if target is out of list bounds 2" <|
            \_ ->
                let
                    x =
                        [ 0, 1, 2]
                in
                Expect.equal [ 0, 1, 2 ] (move 1 1 -3 x)

        ,  test "return unchanged list if source is out of list bounds 1" <|
            \_ ->
                let
                    x =
                        [ 0, 1, 2,3]
                in
                Expect.equal [ 0, 1, 2,3 ] (move 5 1 0 x)

        ,  test "return unchanged list if source is out of list bounds 2" <|
            \_ ->
                let
                    x =
                        [ 0, 1, 2,3]
                in
                Expect.equal [ 0, 1, 2,3 ] (move -1 1 3 x)
        ]
