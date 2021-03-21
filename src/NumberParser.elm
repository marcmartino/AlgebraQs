module NumberParser exposing (numParser)

import Parser exposing ((|.), (|=), Parser, int, oneOf, succeed, symbol)


numParser : Parser Int
numParser =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= int
        , int
        ]
