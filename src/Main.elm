module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html, a)
import Icons exposing (github, moon, sun)
import Palette exposing (backgroundColor, borderRadius, buttonColor, ctaColor, fontColor, moonPurple, secondBackgroundColor, sunOrange)


darkMode =
    True



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    layout [ Background.color (rgb255 238 241 245) ]
        dashboard


edges : { top : number, right : number, bottom : number, left : number }
edges =
    { top = 0, right = 0, bottom = 0, left = 0 }


darkToggle : Bool -> Element msg
darkToggle isDark =
    Input.button
        [ alignTop
        , alignRight
        , Border.rounded 15
        , paddingXY 10 10
        ]
        { onPress = Nothing
        , label =
            if isDark then
                el [ Font.color sunOrange ] (sun |> Element.html)

            else
                el [ Font.color moonPurple ] (moon |> Element.html)
        }


centralForm : Element msg
centralForm =
    let
        header =
            el
                [ centerX
                , Region.heading 1
                , Font.size 24
                , Font.color <| fontColor darkMode
                ]
                (text "Alg Qs")

        inputForm =
            column
                [ padding 10
                , Background.color <| secondBackgroundColor darkMode
                , borderRadius
                , spacingXY 0 10
                ]
                [ -- Input.text []
                  -- { text = "actual text"
                  -- , placeholder = Just (Input.placeholder [] <| text "placeholder text")
                  -- , label = Input.labelHidden "Algebraic Question"
                  -- , onChange = always msg
                  -- }
                  el [ width fill, Background.color <| rgb255 255 255 255, borderRadius, padding 5 ] <| text "|"
                , row [ width fill, spacingXY 10 0 ]
                    [ Input.button
                        [ width fill
                        , Background.color <| ctaColor darkMode
                        , borderRadius
                        , padding 5
                        , alignLeft
                        ]
                        { onPress = Nothing, label = el [ centerX ] <| text "Go" }
                    , Input.button
                        [ width fill
                        , Background.color <| buttonColor darkMode
                        , borderRadius
                        , padding 5
                        , alignRight
                        ]
                        { onPress = Nothing, label = el [ centerX ] <| text "Random" }
                    ]
                ]
    in
    textColumn
        [ centerX
        , centerY
        , paddingXY 0 20
        , spacingXY 0 10
        , width <| px 400
        ]
        [ header
        , inputForm
        ]


answers : Element msg
answers =
    column
        [ centerX
        , centerY
        , width <| px 400
        , padding 10
        , Background.color <| secondBackgroundColor darkMode
        , borderRadius
        , spacingXY 0 10
        , Font.color <| fontColor darkMode
        ]
        [ row []
            [ text "Q: ", text "What is three plus 17?" ]
        , row []
            [ text "A: ", text "Twenty" ]
        ]


footer : Element msg
footer =
    row
        [ alignBottom
        , centerX
        , width fill
        , padding 20
        , Font.color <| fontColor darkMode
        ]
        [ newTabLink [ alignLeft ]
            { url = "//github.com/marcmartino"
            , label = row [] [ github |> Element.html, text "Marc Martino" ]
            }
        , newTabLink [ alignRight ]
            { url = "//github.com/marcmartino/AlgebraQs"
            , label = row [] [ github |> Element.html, text "AlgebraQs" ]
            }
        ]


dashboard : Element msg
dashboard =
    column [ height fill, width fill, Background.color <| backgroundColor darkMode ]
        [ darkToggle darkMode
        , centralForm
        , answers
        , footer
        ]


btn : Element a
btn =
    Input.button
        [ alignTop
        , alignRight
        , focused
            [ Background.color (rgb255 25 45 91)
            , Font.color <| fontColor darkMode
            ]
        ]
        { label = el [] (text "COMPARE")
        , onPress = Nothing
        }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
