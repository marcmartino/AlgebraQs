module ExampleStatementGenerator exposing (ExampleStatement, generateStatement, prettyExampleStatement)

import Array exposing (Array)
import Random
import WrittenOutNumber exposing (writeOut)


type alias ExampleStatement =
    { scalars : List String
    , operators : List String
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
    Random.int -1000000 1000000
        |> Random.map String.fromInt


generateStatement : Random.Generator ExampleStatement
generateStatement =
    Random.int 2 4
        |> Random.andThen (\l -> Random.map2 ExampleStatement (Random.list l scalar) (Random.list (l - 1) operator))


prettyExampleStatement : ExampleStatement -> String
prettyExampleStatement { scalars, operators } =
    let
        scalarOpPairs : List ( String, String )
        scalarOpPairs =
            List.map2 Tuple.pair scalars (List.append operators [ "" ])

        combinedValues : List String
        combinedValues =
            List.indexedMap
                (\index ( num, op ) ->
                    case index of
                        0 ->
                            num
                                ++ " "
                                ++ op

                        _ ->
                            " "
                                ++ num
                                ++ (if op /= "" then
                                        " " ++ op

                                    else
                                        ""
                                   )
                )
                scalarOpPairs
    in
    "What is " ++ String.join " " combinedValues ++ "?"
