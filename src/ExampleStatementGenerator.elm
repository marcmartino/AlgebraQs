module ExampleStatementGenerator exposing (ExampleStatement, generateStatement, prettyExampleStatement)

import Array exposing (Array)
import Random


type alias ExampleStatement =
    { x : String
    , op1 : String
    , y : String
    , op2 : String
    , z : String
    }


exampleOperators : Array String
exampleOperators =
    Array.fromList
        [ "plus"
        , "+"
        , "minus"
        , "-"
        , "times"
        , "*"
        , "multiplied by"
        , "divided by"
        , "/"
        , "^"
        ]


operator : Random.Generator String
operator =
    Random.int 0 (Array.length exampleOperators |> (+) -1)
        |> Random.map (\n -> Array.get n exampleOperators |> Maybe.withDefault "plus")


scalar : Random.Generator String
scalar =
    Random.int -90000000 90000000
        |> Random.map String.fromInt


generateStatement : Random.Generator ExampleStatement
generateStatement =
    Random.map5 ExampleStatement scalar operator scalar operator scalar


prettyExampleStatement : ExampleStatement -> String
prettyExampleStatement { x, op1, y, op2, z } =
    String.join " " [ "What is", x, op1, y, op2, z ++ "?" ]
