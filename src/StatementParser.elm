module StatementParser exposing (answer, toNumeralEquation)

import Html.Attributes exposing (list)
import NumberParser exposing (numParser)
import Parser exposing ((|.), (|=), Parser, keyword, oneOf, run, spaces, succeed, symbol)


type Operator
    = Addition
    | Subtraction
    | Multiplication
    | Division
    | Exponent


type alias AlgebraicStatement =
    { x : Int
    , operation : Operator
    , y : StatementValue
    }


type StatementValue
    = Statement AlgebraicStatement
    | Num Int


numthParser : Parser Int
numthParser =
    Parser.andThen thParser numParser


thParser : Int -> Parser Int
thParser x =
    Parser.map (always x)
        (oneOf
            [ keyword "th"
            , keyword "st"
            , keyword "nd"
            , keyword "rd"
            , symbol ""
            ]
        )


operationParser : Parser Operator
operationParser =
    oneOf
        [ Parser.map (\_ -> Addition) <| keyword "plus"
        , Parser.map (\_ -> Addition) <| keyword "+"
        , Parser.map (\_ -> Subtraction) <| keyword "minus"
        , Parser.map (\_ -> Subtraction) <| keyword "-"
        , Parser.map (\_ -> Multiplication) <| keyword "times"
        , Parser.map (\_ -> Multiplication) <| keyword "*"
        , Parser.map (\_ -> Multiplication) <| keyword "multiplied by"
        , Parser.map (\_ -> Division) <| keyword "divided by"
        , Parser.map (\_ -> Division) <| keyword "/"
        , Parser.map (\_ -> Exponent) <| keyword "to the"
        , Parser.map (\_ -> Exponent) <| keyword "^"
        ]


parseSubsequentValues : Int -> Parser StatementValue
parseSubsequentValues x =
    oneOf
        [ Parser.map (\_ -> Num x) (symbol "?")
        , succeed (\op y -> Statement (AlgebraicStatement x op y))
            |. spaces
            |= operationParser
            |. spaces
            |= Parser.lazy (\_ -> naryParser)
        ]


naryParser : Parser StatementValue
naryParser =
    Parser.andThen parseSubsequentValues numthParser


statementParser : Parser StatementValue
statementParser =
    succeed identity
        |. oneOf
            [ keyword "What is"
            , keyword "What's"
            , keyword "what is"
            , keyword "what's"
            , symbol ""
            ]
        |. oneOf
            [ spaces
            , symbol ""
            ]
        |= naryParser


operatorSimplifiers : List (AlgebraicStatement -> StatementValue)
operatorSimplifiers =
    [ simplifyStatement (\op -> op == Exponent) (\x _ y -> x ^ y)
    , simplifyStatement (\op -> op == Multiplication || op == Division)
        (\x op y ->
            case op of
                Multiplication ->
                    x * y

                Division ->
                    x // y

                _ ->
                    -1
        )
    , simplifyStatement (\op -> op == Addition || op == Subtraction)
        (\x op y ->
            case op of
                Addition ->
                    x + y

                Subtraction ->
                    x - y

                _ ->
                    -1
        )
    ]


simplify : List (AlgebraicStatement -> StatementValue) -> AlgebraicStatement -> Maybe Int
simplify simps alg =
    case simps of
        [] ->
            Nothing

        simp :: otherSimps ->
            case simp alg of
                Num n ->
                    Just n

                Statement stmt ->
                    simplify otherSimps stmt


prepQuestion : String -> String
prepQuestion =
    String.trim
        >> (\str ->
                if String.endsWith "?" str then
                    str

                else
                    str ++ "?"
           )


answer : String -> Maybe Int
answer problem =
    case run statementParser <| prepQuestion <| problem of
        Ok (Statement stmt) ->
            stmt
                |> simplify operatorSimplifiers

        Ok (Num num) ->
            Just num

        _ ->
            Nothing


prettyOperator : Operator -> String
prettyOperator op =
    case op of
        Addition ->
            "+"

        Subtraction ->
            "-"

        Multiplication ->
            "×"

        Division ->
            "÷"

        Exponent ->
            "^"


group : Int -> List a -> List (List a)
group groupSize list =
    let
        nextGroup =
            List.take groupSize list

        remainingItems =
            List.drop groupSize list

        remainingSize =
            List.length remainingItems
    in
    if remainingSize > groupSize then
        nextGroup :: group groupSize remainingItems

    else if remainingSize == 0 then
        [ nextGroup ]

    else
        [ nextGroup, remainingItems ]


prettyNum : Int -> String
prettyNum num =
    abs num
        |> String.fromInt
        |> String.reverse
        |> String.toList
        |> List.map String.fromChar
        |> group 3
        |> List.map (String.join "")
        |> String.join ","
        |> String.reverse
        |> (\pretty ->
                if num < 0 then
                    "-" ++ pretty

                else
                    pretty
           )


toEquation : AlgebraicStatement -> String
toEquation { x, operation, y } =
    case y of
        Num yNum ->
            String.join " " [ prettyNum x, prettyOperator operation, prettyNum yNum ]

        Statement yStatement ->
            String.join " " [ prettyNum x, prettyOperator operation, toEquation yStatement ]


toNumeralEquation : String -> Maybe String
toNumeralEquation problem =
    case run statementParser <| prepQuestion problem of
        Ok (Statement stmt) ->
            toEquation stmt
                |> Just

        Ok (Num num) ->
            prettyNum num
                |> Just

        _ ->
            Nothing


simplifyStatement : (Operator -> Bool) -> (Int -> Operator -> Int -> Int) -> AlgebraicStatement -> StatementValue
simplifyStatement opPred resolveArithmetic alg =
    let
        opMatches =
            opPred alg.operation
    in
    case alg.y of
        Num yNum ->
            if opMatches then
                Num <| resolveArithmetic alg.x alg.operation yNum

            else
                Statement alg

        Statement yStatement ->
            if opMatches then
                simplifyStatement opPred
                    resolveArithmetic
                    { yStatement
                        | x = resolveArithmetic alg.x alg.operation yStatement.x
                    }

            else
                Statement
                    { alg
                        | y = simplifyStatement opPred resolveArithmetic yStatement
                    }
