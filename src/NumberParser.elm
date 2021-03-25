module NumberParser exposing (numParser)

import Parser exposing ((|.), (|=), Parser, int, keyword, oneOf, spaces, succeed, symbol)
import WrittenNumbers exposing (mapSingleDigitNames, mapTeensDigitNames, mapTensDigitNames)


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
        [ succeed negate
            |. oneOf
                [ symbol "-"
                , succeed ()
                    |. keyword "negative"
                    |. spaces
                ]
            |= oneOf
                [ int
                , threeDigitNumParser
                ]
        , int
        , threeDigitNumParser
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


subsequentHundredParser : Int -> Parser Int
subsequentHundredParser n =
    let
        checkForAnd : Int -> Parser Int
        checkForAnd x =
            succeed x
                |. spaces
                |. oneOf
                    [ succeed ()
                        |. keyword "and"
                        |. spaces
                    , symbol ""
                    ]
    in
    succeed identity
        |. spaces
        |= oneOf
            [ Parser.map (always <| n * 100) (symbol "hundred")
                |> Parser.andThen checkForAnd
                |> Parser.andThen
                    (\x ->
                        succeed ((+) x)
                            |= oneOf
                                [ twoDigitNumParser
                                , oneOf singleDigitParsers
                                , Parser.map (always 0) (symbol "")
                                ]
                    )
            , Parser.map (always n) (symbol "")
            ]


threeDigitNumParser : Parser Int
threeDigitNumParser =
    oneOf
        [ twoDigitNumParser
        , oneOf singleDigitParsers
            |> Parser.andThen subsequentHundredParser
        ]
