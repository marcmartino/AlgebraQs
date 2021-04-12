module StatementVizualizersTests exposing (tests)

import Expect
import StatementParser
    exposing
        ( StatementValue(..)
        , parseStatement
        )
import StatementVizualizers exposing (toNumeralEquation)
import Test exposing (..)


toStatement : String -> StatementValue
toStatement =
    parseStatement >> Result.withDefault (Num 0)


tests : Test
tests =
    describe "Statement Parsers"
        [ describe "Parsing to Numeral Equation"
            [ test "simple" <|
                \() -> Expect.equal "1" <| toNumeralEquation <| toStatement "What is 1?"
            , test "two numbers" <|
                \() -> Expect.equal "1 + 2" <| toNumeralEquation <| toStatement "What is 1 + 2?"
            , test "long number simple statement" <|
                \() ->
                    Expect.equal "-899,933" <|
                        toNumeralEquation <|
                            toStatement
                                "What is negative eight hundred and ninety-nine thousand nine hundred and thirty-three?"
            , test "long written out example" <|
                \() ->
                    Expect.equal "185,589 - -311,820 × 638,519 × -899,933" <|
                        toNumeralEquation <|
                            toStatement
                                ("What is "
                                    ++ "one hundred and eighty-five thousand five hundred and eighty-nine "
                                    ++ "minus "
                                    ++ "negative three hundred and eleven thousand eight hundred and twenty "
                                    ++ "times "
                                    ++ "six hundred and thirty-eight thousand five hundred and nineteen "
                                    ++ "times "
                                    ++ "negative eight hundred and ninety-nine thousand nine hundred and thirty-three?"
                                )
            , test "with exponent" <|
                \() -> Expect.equal "2³" <| toNumeralEquation <| toStatement "What is 2 to the 3rd?"
            , test "with exponent in the middle" <|
                \() -> Expect.equal "2³ + 7" <| toNumeralEquation <| toStatement "What is 2 to the 3rd plus 7?"
            , test "with negative exponent" <|
                \() -> Expect.equal "2⁻⁴" <| toNumeralEquation <| toStatement "What is 2 to the -4?"
            ]
        ]
