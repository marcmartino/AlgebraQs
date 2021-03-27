module WrittenOutNumberTests exposing (tests)

import Expect
import Test exposing (..)
import WrittenOutNumber exposing (WrittenNumError(..), writeOut)


tests : Test
tests =
    describe "Series"
        [ test "one" <|
            \() ->
                Expect.equal (Ok "one")
                    (writeOut 1)
        , test "fourteen" <|
            \() ->
                Expect.equal (Ok "fourteen")
                    (writeOut 14)
        , test "ten" <|
            \() ->
                Expect.equal (Ok "ten")
                    (writeOut 10)
        , test "twenty" <|
            \() ->
                Expect.equal (Ok "twenty")
                    (writeOut 20)
        , test "twenty-two" <|
            \() ->
                Expect.equal (Ok "twenty-two")
                    (writeOut 22)
        , test "one hundred" <|
            \() ->
                Expect.equal (Ok "one hundred")
                    (writeOut 100)
        , test "one hundred twenty" <|
            \() ->
                Expect.equal (Ok "one hundred and twenty")
                    (writeOut 120)
        , test "one hundred twenty-three" <|
            \() ->
                Expect.equal (Ok "one hundred and twenty-three")
                    (writeOut 123)
        , test "one thousand" <|
            \() ->
                Expect.equal (Ok "one thousand")
                    (writeOut 1000)
        , test "one thousand two hundred thirty-four" <|
            \() ->
                Expect.equal (Ok "one thousand two hundred and thirty-four")
                    (writeOut 1234)
        , test "one million" <|
            \() ->
                Expect.equal (Ok "one million")
                    (writeOut 1000000)
        , test "1002345" <|
            \() ->
                Expect.equal (Ok "one million two thousand three hundred and forty-five")
                    (writeOut 1002345)
        , test "one billion" <|
            \() ->
                Expect.equal (Ok "one billion")
                    (writeOut 1000000000)
        , test "number too large" <|
            \() ->
                Expect.equal (Err TooLarge)
                    (writeOut 10000000000000000)
        , test "negative number" <|
            \() ->
                Expect.equal (Ok "negative forty-two")
                    (writeOut -42)
        , test "zero" <|
            \() ->
                Expect.equal (Ok "zero")
                    (writeOut 0)
        , test "987654321123" <|
            \() ->
                Expect.equal
                    (Ok
                        ("nine hundred and eighty-seven billion "
                            ++ "six hundred and fifty-four million "
                            ++ "three hundred and twenty-one thousand "
                            ++ "one hundred and twenty-three"
                        )
                    )
                    (writeOut 987654321123)
        ]
