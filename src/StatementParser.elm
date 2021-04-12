module StatementParser exposing
    ( AlgebraicStatement
    , Operator(..)
    , StatementParserResult
    , StatementValue(..)
    , parseStatement
    , prepQuestion
    , statementParser
    )

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



-- answer : String -> Maybe Int
-- answer problem =
--     case run statementParser <| prepQuestion <| problem of
--         Ok (Statement stmt) ->
--             simplifyOps stmt
--         Ok (Num num) ->
--             Just num
--         _ ->
--             Nothing


parseStatement : String -> StatementParserResult
parseStatement =
    prepQuestion >> run statementParser
