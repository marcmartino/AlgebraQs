module WrittenNumbers exposing (digitGroupName, mapGroupNames, mapSingleDigitNames, mapTeensDigitNames, mapTensDigitNames, singleDigitName, teensDigitName, tensDigitName)

import Array exposing (Array, fromList)
import List


type alias DigitMap =
    (String -> a) -> List a


singleDigitsList : List String
singleDigitsList =
    [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" ]


singleDigits : Array String
singleDigits =
    fromList singleDigitsList


teenDigitsList : List String
teenDigitsList =
    [ "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "ninteen" ]


teenDigits : Array String
teenDigits =
    fromList teenDigitsList


tensDigitList : List String
tensDigitList =
    [ "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "nintey" ]


tensDigits : Array String
tensDigits =
    fromList tensDigitList


groupingNamesList : List String
groupingNamesList =
    [ "thousand", "million", "billion", "trillion", "quadrillion", "quintrillion", "sentillion", "septillion" ]


groupingNames : Array String
groupingNames =
    fromList <| "" :: groupingNamesList


singleDigitName : Int -> Maybe String
singleDigitName x =
    Array.get (x - 1) singleDigits


teensDigitName : Int -> Maybe String
teensDigitName x =
    Array.get (x // 10 - 1) teenDigits


tensDigitName : Int -> Maybe String
tensDigitName x =
    Array.get (x - 1) tensDigits


digitGroupName : Int -> Maybe String
digitGroupName x =
    Array.get x groupingNames


listMap : List a -> (a -> b) -> List b
listMap xs fn =
    List.map fn xs


mapSingleDigitNames : DigitMap
mapSingleDigitNames =
    listMap singleDigitsList


mapTeensDigitNames : DigitMap
mapTeensDigitNames =
    listMap teenDigitsList


mapTensDigitNames : DigitMap
mapTensDigitNames =
    listMap tensDigitList


mapGroupNames : DigitMap
mapGroupNames =
    listMap groupingNamesList
