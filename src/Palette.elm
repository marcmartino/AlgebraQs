module Palette exposing (..)

import Element exposing (Color, rgb255, toRgb)
import Element.Border as Border


borderRadius : Element.Attribute msg
borderRadius =
    Border.rounded 5


boxShadow : Bool -> Element.Attr decorative msg
boxShadow dark =
    Border.shadow
        { color =
            if dark then
                celadonBlue

            else
                opal
        , offset = ( 2, 2 )
        , blur = 3
        , size = 4
        }



-- contrast : Color -> Color -> Float
-- contrast x y =
--     let
--         xRgb = toRgb x
--         yRgb = toRgb y
--     in
--     xRgb.red - yRgb.red


fontColor : Bool -> Color
fontColor dark =
    if dark then
        honeydew

    else
        prussianBlue



-- maximumContrast bgColor
-- [ honeydew
-- , celadonBlue
-- ]


backgroundColor : Bool -> Color
backgroundColor dark =
    if dark then
        prussianBlue

    else
        honeydew


secondBackgroundColor : Bool -> Color
secondBackgroundColor dark =
    if dark then
        indigoDye

    else
        powderBlue


ctaColor : Bool -> Color
ctaColor dark =
    purpleMountain


buttonColor : Bool -> Color
buttonColor dark =
    mauvelous


opal : Color
opal =
    rgb255 201 219 219


sunOrange : Color
sunOrange =
    rgb255 201 157 24


indigoDye : Color
indigoDye =
    rgb255 50 71 102


moonPurple : Color
moonPurple =
    rgb255 74 24 201


honeydew : Color
honeydew =
    rgb255 241 250 238


powderBlue : Color
powderBlue =
    rgb255 168 218 220


celadonBlue : Color
celadonBlue =
    rgb255 69 123 157


prussianBlue : Color
prussianBlue =
    rgb255 29 53 87


mauvelous : Color
mauvelous =
    rgb255 244 164 171


purpleMountain : Color
purpleMountain =
    rgb255 153 124 202
