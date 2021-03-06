module Palette exposing (Theme, ThemeName(..), boxShadow, getTheme)

import Element exposing (Color, rgb255, rgba255)
import Element.Border as Border


type ThemeName
    = Dark
    | Light


type alias Theme =
    { name : ThemeName
    , borderRadius : Int
    , textColor : Color
    , errorTextColor : Color
    , secondaryFontColor : Color
    , disabledFontColor : Color
    , backgroundColor : Color
    , disabledBackgroundColor : Color
    , sectionBackgroundColor : Color
    , ctaColor : Color
    , ctaFocusedColor : Color
    , buttonColor : Color
    , buttonFocusedColor : Color
    , buttonTextColor : Color
    , themeToggleButtonColor : Color
    }


getTheme : ThemeName -> Theme
getTheme name =
    if name == Light then
        lightTheme

    else
        darkTheme


lightTheme : Theme
lightTheme =
    { name = Light
    , borderRadius = 5
    , textColor = prussianBlue
    , errorTextColor = mauvelous
    , secondaryFontColor = mauvelous
    , disabledFontColor = disabledGray
    , backgroundColor = honeydew
    , disabledBackgroundColor = disabledBackgroundGray
    , sectionBackgroundColor = powderBlue
    , ctaColor = purpleMountain
    , ctaFocusedColor = purpleMountain70
    , buttonColor = mauvelous
    , buttonFocusedColor = mauvelous70
    , buttonTextColor = prussianBlue
    , themeToggleButtonColor = moonPurple
    }


darkTheme : Theme
darkTheme =
    { name = Dark
    , borderRadius = 5
    , textColor = honeydew
    , errorTextColor = mauvelous
    , secondaryFontColor = mauvelous
    , disabledFontColor = disabledGray
    , backgroundColor = oxfordBlue
    , disabledBackgroundColor = disabledBackgroundGray
    , sectionBackgroundColor = prussianBlue
    , ctaColor = sizzlingRed
    , ctaFocusedColor = rubyRed
    , buttonColor = purpleMountain
    , buttonFocusedColor = darkSlateBlue
    , buttonTextColor = rgb255 220 220 220
    , themeToggleButtonColor = sunOrange
    }


boxShadow : ThemeName -> Element.Attr decorative msg
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


opal : Color
opal =
    rgb255 201 219 219


sunOrange : Color
sunOrange =
    rgb255 201 157 24


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


oxfordBlue : Color
oxfordBlue =
    rgb255 15 28 46


mauvelous : Color
mauvelous =
    rgb255 244 164 171


mauvelous70 : Color
mauvelous70 =
    rgba255 244 164 171 0.7


purpleMountain : Color
purpleMountain =
    rgb255 153 124 202


purpleMountain70 : Color
purpleMountain70 =
    rgba255 153 124 202 0.7


disabledBackgroundGray : Color
disabledBackgroundGray =
    rgb255 150 150 150


disabledGray : Color
disabledGray =
    rgb255 110 110 110


sizzlingRed : Color
sizzlingRed =
    rgb255 233 73 87


rubyRed : Color
rubyRed =
    rgb255 163 20 32


darkSlateBlue : Color
darkSlateBlue =
    rgb255 74 57 147
