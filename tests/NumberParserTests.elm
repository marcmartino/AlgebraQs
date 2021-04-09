module NumberParserTests exposing (tests)

import Expect
import NumberParser exposing (numParser)
import Parser exposing (run)
import Test exposing (..)


tests : Test
tests =
    describe "numParser"
        [ describe "single words"
            [ test "1" <|
                \() ->
                    Expect.equal (Ok 1)
                        (run numParser "1")
            , test "one" <|
                \() ->
                    Expect.equal (Ok 1)
                        (run numParser "one")
            , test "two" <|
                \() ->
                    Expect.equal (Ok 2)
                        (run numParser "two")
            , test "three" <|
                \() ->
                    Expect.equal (Ok 3)
                        (run numParser "three")
            , test "4" <|
                \() ->
                    Expect.equal (Ok 4)
                        (run numParser "4")
            , test "four" <|
                \() ->
                    Expect.equal (Ok 4)
                        (run numParser "four")
            , test "five" <|
                \() ->
                    Expect.equal (Ok 5)
                        (run numParser "five")
            , test "six" <|
                \() ->
                    Expect.equal (Ok 6)
                        (run numParser "six")
            , test "seven" <|
                \() ->
                    Expect.equal (Ok 7)
                        (run numParser "seven")
            , test "eight" <|
                \() ->
                    Expect.equal (Ok 8)
                        (run numParser "eight")
            , test "nine" <|
                \() ->
                    Expect.equal (Ok 9)
                        (run numParser "nine")
            , test "ten" <|
                \() ->
                    Expect.equal (Ok 10)
                        (run numParser "ten")
            , test "eleven" <|
                \() ->
                    Expect.equal (Ok 11)
                        (run numParser "eleven")
            , test "twelve" <|
                \() ->
                    Expect.equal (Ok 12)
                        (run numParser "twelve")
            , test "thirteen" <|
                \() ->
                    Expect.equal (Ok 13)
                        (run numParser "thirteen")
            , test "fourteen" <|
                \() ->
                    Expect.equal (Ok 14)
                        (run numParser "fourteen")
            , test "fifteen" <|
                \() ->
                    Expect.equal (Ok 15)
                        (run numParser "fifteen")
            , test "sixteen" <|
                \() ->
                    Expect.equal (Ok 16)
                        (run numParser "sixteen")
            , test "seventeen" <|
                \() ->
                    Expect.equal (Ok 17)
                        (run numParser "seventeen")
            , test "eighteen" <|
                \() ->
                    Expect.equal (Ok 18)
                        (run numParser "eighteen")
            , test "nineteen" <|
                \() ->
                    Expect.equal (Ok 19)
                        (run numParser "nineteen")
            , test "twenty" <|
                \() ->
                    Expect.equal (Ok 20)
                        (run numParser "twenty")
            , test "thirty" <|
                \() ->
                    Expect.equal (Ok 30)
                        (run numParser "thirty")
            , test "forty" <|
                \() ->
                    Expect.equal (Ok 40)
                        (run numParser "forty")
            , test "fifty" <|
                \() ->
                    Expect.equal (Ok 50)
                        (run numParser "fifty")
            , test "sixty" <|
                \() ->
                    Expect.equal (Ok 60)
                        (run numParser "sixty")
            , test "seventy" <|
                \() ->
                    Expect.equal (Ok 70)
                        (run numParser "seventy")
            , test "eighty" <|
                \() ->
                    Expect.equal (Ok 80)
                        (run numParser "eighty")
            , test "ninety" <|
                \() ->
                    Expect.equal (Ok 90)
                        (run numParser "ninety")
            ]
        , describe "negative words"
            [ test "-1" <|
                \() ->
                    Expect.equal (Ok -1)
                        (run numParser "-1")
            , test "negative five" <|
                \() -> Expect.equal (Ok -5) (run numParser "negative five")
            ]
        , describe "suffixed numbers"
            [ test "zeroth" <|
                \() ->
                    Expect.equal (Ok 0)
                        (run numParser "zeroth")
            , test "first" <|
                \() ->
                    Expect.equal (Ok 1)
                        (run numParser "first")
            , test "second" <|
                \() ->
                    Expect.equal (Ok 2)
                        (run numParser "second")
            , test "third" <|
                \() ->
                    Expect.equal (Ok 3)
                        (run numParser "third")
            , test "fifth" <|
                \() ->
                    Expect.equal (Ok 5)
                        (run numParser "fifth")
            ]
        , describe "multiple word numbers"
            [ test "twenty-seven" <|
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
            , test "negative two hundred and seventeen million" <|
                \() ->
                    Expect.equal (Ok -217000000)
                        (run numParser "negative two hundred and seventeen million")
            , test "negative eight hundred and nintey-nine thousand nine hundred and thirty-three" <|
                \() ->
                    Expect.equal (Ok -899933)
                        (run numParser "negative eight hundred and ninety-nine thousand nine hundred and thirty-three")
            , test "eight hundred and ninety-nine thousand nine hundred and thirty-three" <|
                \() ->
                    Expect.equal (Ok 899933)
                        (run numParser "eight hundred and ninety-nine thousand nine hundred and thirty-three")
            , test "seventy million" <|
                \() ->
                    Expect.equal (Ok 70000000)
                        (run numParser "seventy million")
            , test "one thousand three hundred" <|
                \() ->
                    Expect.equal (Ok 1300)
                        (run numParser "one thousand three hundred")
            , test "negative six hundred and thirty-seven million seven thousand fifty-nine" <|
                \() ->
                    Expect.equal (Ok -637007059)
                        (run numParser "negative six hundred and thirty-seven million seven thousand fifty-nine")
            ]
        ]
