module StatementParserTests exposing (tests)

import Expect
import Parser exposing (run)
import StatementParser exposing (StatementValue(..), prepQuestion, statementParser)
import StatementVizualizers exposing (simplifyOps)
import Test exposing (..)


answer : String -> Maybe Int
answer problem =
    case run statementParser <| prepQuestion <| problem of
        Ok (Statement stmt) ->
            simplifyOps stmt

        Ok (Num num) ->
            Just num

        _ ->
            Nothing


tests : Test
tests =
    describe "Statement Parsers"
        [ describe "Statement Parsing"
            [ test "just a number" <|
                \() ->
                    Expect.equal (Just 5) <| answer "What is 5?"
            , test "addition" <|
                \() ->
                    Expect.equal (Just 2) <| answer "What is 1 plus 1?"
            , test "more addition" <|
                \() ->
                    Expect.equal (Just 55) <| answer "What is 53 plus 2?"
            , test "addition with negative numbers" <|
                \() ->
                    Expect.equal (Just -11) <| answer "What is -1 plus -10?"
            , test "large addition" <|
                \() ->
                    Expect.equal (Just 45801) <| answer "What is 123 plus 45678?"
            , test "subtraction" <|
                \() ->
                    Expect.equal (Just 16) <| answer "What is 4 minus -12?"
            , test "multiplication" <|
                \() ->
                    Expect.equal (Just -75) <| answer "What is -3 multiplied by 25?"
            , test "division" <|
                \() ->
                    Expect.equal (Just -11) <| answer "What is 33 divided by -3?"
            , test "multiple additions" <|
                \() ->
                    Expect.equal (Just 20) <| answer "What is 1 plus 12 plus 3 plus 4?"
            , test "addition and subtraction" <|
                \() ->
                    Expect.equal (Just 8) <| answer "What is 1 plus 5 minus -2?"
            , test "multiple subtraction" <|
                \() ->
                    Expect.equal (Just 3) <| answer "What is 20 minus 4 minus 13?"
            , test "subtraction then addition" <|
                \() ->
                    Expect.equal (Just 14) <| answer "What is 17 minus 6 plus 3?"
            , test "multiple multiplication" <|
                \() ->
                    Expect.equal (Just -12) <| answer "What is 2 multiplied by -2 multiplied by 3?"
            , test "addition and multiplication" <|
                \() ->
                    Expect.equal (Just -17) <| answer "What is -3 plus 7 multiplied by -2?"
            , test "addition and multiplication with symbols" <|
                \() ->
                    Expect.equal (Just -17) <| answer "What is -3 + 7 * -2?"
            , test "multiple division" <|
                \() ->
                    Expect.equal (Just -6) <| answer "What is -12 divided by 2?"
            , test "powers" <|
                \() ->
                    Expect.equal (Just 10) <| answer "What is 3 to the 2nd + 1?"
            , test "different opening question" <|
                \() ->
                    Expect.equal (Just 10) <| answer "3 to the 2nd + 1?"
            , test "five calculations" <|
                \() ->
                    Expect.equal (Just 24) <| answer "7 plus 3 to the 2nd + 1 times 8?"
            , test "simple written out numbers" <|
                \() ->
                    Expect.equal (Just 10) <| answer "seven plus three?"
            , test "written out numbers that contains an order of magnitude" <|
                \() ->
                    Expect.equal (Just 1000200) <| answer "What is one hundred times two plus one million?"
            , test "large written out single number" <|
                \() ->
                    Expect.equal (Just -899933) <| answer "What is negative eight hundred and ninety-nine thousand nine hundred and thirty-three?"
            , test "numeric exponent" <|
                \() ->
                    Expect.equal (Just 8) <| answer "What is 2 ^ 3?"
            , test "written exponent" <|
                \() ->
                    Expect.equal (Just 8) <| answer "What is two ^ three?"
            , test "written exponent w/ 'to the' operator" <|
                \() ->
                    Expect.equal (Just 8) <| answer "What is two to the third?"
            , test "written out exponent w/ 'to the x power' operator" <|
                \() ->
                    Expect.equal (Just 8) <| answer "What is two to the third power?"
            , test "numeric exponent w/ rd suffx" <|
                \() ->
                    Expect.equal (Just 8) <| answer "What is 2 to the 3rd?"
            , test "numeric exponent w/ rd suffx with subsequent 'power' " <|
                \() ->
                    Expect.equal (Just 8) <| answer "What is 2 to the 3rd?"
            , test "numeric exponent w/ nd suffix" <|
                \() ->
                    Expect.equal (Just 4) <| answer "What is 2 to the 2nd?"
            , test "numeric exponent w/ st suffix" <|
                \() ->
                    Expect.equal (Just 2097152) <| answer "What is 2 to the 21st?"
            , test "numeric exponent w/ th suffix" <|
                \() ->
                    Expect.equal (Just 32) <| answer "What is 2 to the 5th?"
            , test "unknown operation" <|
                \() ->
                    Expect.equal Nothing <| answer "What is 52 cubed?"
            , test "Non math question" <|
                \() ->
                    Expect.equal Nothing <| answer "Who is the President of the United States?"
            , test "reject problem missing an operand" <|
                \() ->
                    Expect.equal Nothing <| answer "What is 1 plus?"
            , test "reject problem with no operands or operators" <|
                \() ->
                    Expect.equal Nothing <| answer "What is?"
            , test "reject two operations in a row" <|
                \() ->
                    Expect.equal Nothing <| answer "What is 1 plus plus 2?"
            , test "reject two numbers in a row" <|
                \() ->
                    Expect.equal Nothing <| answer "What is 1 plus 2 1?"
            , test "reject postfix notation" <|
                \() ->
                    Expect.equal Nothing <| answer "What is 1 2 plus?"
            , test "reject prefix notation" <|
                \() ->
                    Expect.equal Nothing <| answer "What is plus 1 2?"
            ]
        ]
