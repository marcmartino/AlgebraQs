module NumberParser exposing (numParser)

import Parser exposing ((|.), (|=), Parser, int, keyword, oneOf, spaces, succeed, symbol)
import WrittenNumbers exposing (mapGroupNames, mapSingleDigitNames, mapTeensDigitNames, mapTensDigitNames, thNumberNames)


singleDigitParsers : List (Parser Int)
singleDigitParsers =
    let
        singleDigitParser : ( Int, String ) -> Parser Int
        singleDigitParser ( num, token ) =
            Parser.map (always num) (keyword token)
    in
    mapSingleDigitNames singleDigitParser


numParser : Parser Int
numParser =
    oneOf
        [ writtenOutNumParser
        , succeed negate
            |. oneOf
                [ symbol "-"
                , succeed ()
                    |. keyword "negative"
                    |. spaces
                ]
            |= oneOf
                [ writtenOutNumParser
                , int
                ]
        , int
        ]


twoDigitNumParser : Parser Int
twoDigitNumParser =
    let
        teensParser =
            oneOf (mapTeensDigitNames <| \( num, token ) -> Parser.map (always num) (keyword token))

        tensParser =
            oneOf (mapTensDigitNames <| \( num, token ) -> Parser.map (always num) (keyword token))
    in
    oneOf
        [ teensParser
        , Parser.andThen
            (\tens ->
                oneOf
                    [ succeed ((+) tens)
                        |. symbol "-"
                        |= oneOf singleDigitParsers
                    , Parser.map (always tens) (symbol "")
                    ]
            )
            tensParser
        ]


singleDigitHundreds : Parser Int
singleDigitHundreds =
    let
        hundredAnds : Parser Int
        hundredAnds =
            oneOf <| mapSingleDigitNames (\( num, name ) -> Parser.map (always (num * 100)) (symbol (name ++ " hundred and")))

        hundreds =
            oneOf <| mapSingleDigitNames (\( num, name ) -> Parser.map (always (num * 100)) (symbol (name ++ " hundred")))
    in
    oneOf
        [ hundredAnds
        , hundreds
        ]
        |> Parser.andThen
            (\n ->
                oneOf
                    [ succeed identity
                        |. spaces
                        |= Parser.map ((+) n)
                            (oneOf
                                [ twoDigitNumParser
                                , oneOf singleDigitParsers
                                , Parser.map (always 0) (symbol "")
                                ]
                            )
                    , Parser.map (always n) (symbol "")
                    ]
            )


writtenOutNumParser : Parser Int
writtenOutNumParser =
    oneOf
        [ oneOf <| thNumberNames (\( val, numName ) -> Parser.map (always val) (keyword numName))
        , oneOf
            [ twoDigitNumParser
            , singleDigitHundreds
            , oneOf singleDigitParsers
            ]
            |> Parser.andThen orderOfMagnitudeParser
            |> Parser.andThen writtenOutNumRecurseParser
        ]


orderOfMagnitudeParser : Int -> Parser Int
orderOfMagnitudeParser prevVal =
    succeed identity
        |. spaces
        |= oneOf
            [ oneOf
                (mapGroupNames <| \( num, token ) -> Parser.map (always <| prevVal * num) (keyword token))
            , Parser.map (always prevVal) (symbol "")
            ]


writtenOutNumRecurseParser : Int -> Parser Int
writtenOutNumRecurseParser prevNum =
    succeed identity
        |. spaces
        |= oneOf
            -- [ Parser.map ((+) x) writtenOutNumParser
            [ writtenOutNumParser
                |> Parser.andThen
                    (\nextNum ->
                        if prevNum > nextNum then
                            succeed (prevNum + nextNum)

                        else
                            Parser.problem "Orders must descend to be a valid number"
                    )
            , Parser.map (always prevNum) (symbol "")
            ]
