module NumberParserTests exposing (tests)

import Expect
import NumberParser exposing (numParser)
import Parser exposing (run)
import Test exposing (..)


tests : Test
tests =
    describe "numParser"
        [ test "1" <|
            \() ->
                Expect.equal (Ok 1)
                    (run numParser "1")
        , test "-1" <|
            \() ->
                Expect.equal (Ok -1)
                    (run numParser "-1")
        , test "one" <|
            \() ->
                Expect.equal (Ok 1)
                    (run numParser "one")
        , test "two" <|
            \() ->
                Expect.equal (Ok 2)
                    (run numParser "two")
        , test "thirteen" <|
            \() ->
                Expect.equal (Ok 13)
                    (run numParser "thirteen")
        , test "twenty" <|
            \() ->
                Expect.equal (Ok 20)
                    (run numParser "twenty")
        , test "twenty-seven" <|
            \() ->
                Expect.equal (Ok 27)
                    (run numParser "twenty-seven")
        ]
