module Palette exposing (..)

import Element exposing (Color, rgb255, rgba255, toRgb)
import Element.Background as Background
import Element.Border as Border


type Theme
    = Dark
    | Light


borderRadius : Element.Attribute msg
borderRadius =
    Border.rounded 5


boxShadow : Theme -> Element.Attr decorative msg
boxShadow theme =
    Border.shadow
        { color =
            if theme == Dark then
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


fontColor : Theme -> Color
fontColor theme =
    if theme == Dark then
        honeydew

    else
        prussianBlue



-- maximumContrast bgColor
-- [ honeydew
-- , celadonBlue
-- ]


backgroundColor : Theme -> Color
backgroundColor theme =
    if theme == Dark then
        prussianBlue

    else
        honeydew


secondBackgroundColor : Theme -> Color
secondBackgroundColor theme =
    if theme == Dark then
        indigoDye

    else
        powderBlue


ctaFocusedColor =
    Background.gradient
        { angle = 125
        , steps =
            [ rgba255 153 124 202 0.7 ]
        }


buttonFocusedColor =
    Background.gradient
        { angle = 125
        , steps =
            [ rgba255 244 164 171 0.7 ]
        }


ctaColor : Theme -> Color
ctaColor theme =
    purpleMountain


buttonColor : Theme -> Color
buttonColor theme =
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
