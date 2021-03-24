module WrittenNumbers exposing (digitGroupName, mapGroupNames, mapSingleDigitNames, mapTeensDigitNames, mapTensDigitNames, singleDigitName, teensDigitName, tensDigitName)

import Array exposing (Array, fromList)
import List


type alias DigitMap a =
    (( Int, String ) -> a) -> List a


singleDigitsList : List String
singleDigitsList =
    [ "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" ]


teenDigitsList : List String
teenDigitsList =
    [ "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "ninteen" ]


tensDigitList : List String
tensDigitList =
    [ "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "nintey" ]


groupingNamesList : List String
groupingNamesList =
    [ "thousand", "million", "billion", "trillion", "quadrillion", "quintrillion", "sentillion", "septillion" ]


singleDigits : Array String
singleDigits =
    fromList singleDigitsList


teenDigits : Array String
teenDigits =
    fromList teenDigitsList


tensDigits : Array String
tensDigits =
    fromList tensDigitList


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


transformListIndex : (Int -> Int) -> List String -> List ( Int, String )
transformListIndex indexFn =
    List.indexedMap (\fst snd -> ( indexFn fst, snd ))


mapSingleDigitNames : DigitMap a
mapSingleDigitNames =
    listMap <| transformListIndex ((+) 1) singleDigitsList


mapTeensDigitNames : DigitMap a
mapTeensDigitNames =
    listMap <| transformListIndex ((+) 10) teenDigitsList


mapTensDigitNames : DigitMap a
mapTensDigitNames =
    listMap <| transformListIndex ((+) 1 >> (*) 10) tensDigitList


mapGroupNames : DigitMap a
mapGroupNames =
    listMap <| transformListIndex identity ("" :: groupingNamesList)
