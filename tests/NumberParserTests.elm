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
        , test "negative twenty-seven" <|
            \() ->
                Expect.equal (Ok -27)
                    (run numParser "negative twenty-seven")
        , test "three hundred" <|
            \() ->
                Expect.equal (Ok 300)
                    (run numParser "three hundred")
        , test "three hundred and two" <|
            \() ->
                Expect.equal (Ok 302)
                    (run numParser "three hundred and two")
        , test "three hundred and twenty" <|
            \() ->
                Expect.equal (Ok 320)
                    (run numParser "three hundred and twenty")
        , test "three hundred thirty" <|
            \() ->
                Expect.equal (Ok 330)
                    (run numParser "three hundred thirty")
        , test "three hundred and twenty-nine" <|
            \() ->
                Expect.equal (Ok 329)
                    (run numParser "three hundred and twenty-nine")
        , test "three hundred thirty-two" <|
            \() ->
                Expect.equal (Ok 332)
                    (run numParser "three hundred thirty-two")
        , test "negative two hundred and seventeen" <|
            \() ->
                Expect.equal (Ok -217)
                    (run numParser "negative two hundred and seventeen")
        ]
