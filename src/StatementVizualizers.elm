module StatementVizualizers exposing
    ( getStatementErrorMessage
    , operatorSimplifiers
    , prettyNum
    , simplifyOps
    , toNumeralEquation
    , toNumericAnswer
    )

import Array exposing (Array)
import Parser
import StatementParser
    exposing
        ( AlgebraicStatement
        , Operator(..)
        , StatementValue(..)
        )


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


superscriptNums : Array String
superscriptNums =
    "⁰¹²³⁴⁵⁶⁷⁸⁹"
        |> String.toList
        |> Array.fromList
        |> Array.map String.fromChar


toSuperscript : Int -> String
toSuperscript num =
    num
        |> abs
        |> String.fromInt
        |> String.toList
        |> List.map
            (String.fromChar
                >> String.toInt
                >> Maybe.withDefault 0
                >> (\i -> Array.get i superscriptNums)
                >> Maybe.withDefault ""
            )
        |> String.join ""
        |> (\ssNum ->
                if num < 0 then
                    "⁻" ++ ssNum

                else
                    ssNum
           )


toEquation : AlgebraicStatement -> String
toEquation { x, operation, y } =
    let
        subsequentY yVal =
            case yVal of
                Num yNum ->
                    prettyNum yNum

                Statement yStmt ->
                    toEquation yStmt
    in
    if operation == Exponent then
        case y of
            Num yNum ->
                prettyNum x ++ toSuperscript yNum

            Statement yStatement ->
                String.join " "
                    [ prettyNum x ++ toSuperscript yStatement.x
                    , prettyOperator yStatement.operation
                    , subsequentY yStatement.y
                    ]

    else
        String.join " " [ prettyNum x, prettyOperator operation, subsequentY y ]


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


simplifyOps : AlgebraicStatement -> Maybe Int
simplifyOps =
    simplify operatorSimplifiers
