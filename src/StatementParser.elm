module StatementParser exposing (StatementParserResult, StatementValue(..), answer, getStatementErrorMessage, parseStatement, prettyNum, toNumeralEquation, toNumericAnswer)

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


type alias StatementParserResult =
    Result (List Parser.DeadEnd) StatementValue


numthParser : Parser Int
numthParser =
    numParser
        |> Parser.andThen thParser
        |> Parser.andThen
            (\n ->
                Parser.map (always n)
                    (oneOf
                        [ keyword " power"
                        , symbol ""
                        ]
                    )
            )


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
            [ keyword "what is"
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
        >> String.toLower
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


toNumericAnswer : StatementValue -> Result String Int
toNumericAnswer statement =
    case statement of
        Statement stmt ->
            case stmt |> simplify operatorSimplifiers of
                Just num ->
                    Ok num

                Nothing ->
                    Err "Number too large to display"

        Num num ->
            Ok num


parseStatement : String -> StatementParserResult
parseStatement =
    prepQuestion >> run statementParser


getStatementErrorMessage : List Parser.DeadEnd -> String
getStatementErrorMessage errors =
    case errors of
        { problem } :: _ ->
            case problem of
                Parser.Problem problemMessage ->
                    problemMessage

                -- it's possible to parse out other issues here and give better errors
                _ ->
                    "Parse error"

        _ ->
            "Parse error"


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


toNumeralEquation : StatementValue -> String
toNumeralEquation statement =
    case statement of
        Statement stmt ->
            toEquation stmt

        Num num ->
            prettyNum num


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
