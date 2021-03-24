module NumberParser exposing (numParser)

import Parser exposing ((|.), (|=), Parser, int, keyword, oneOf, succeed, symbol)
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
            |. symbol "-"
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


threeDigitNumParser : Parser Int
threeDigitNumParser =
    oneOf
        [ twoDigitNumParser
        , oneOf singleDigitParsers
        ]
