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
import Palette exposing (Theme(..), backgroundColor, borderRadius, boxShadow, buttonColor, buttonFocusedColor, ctaColor, ctaFocusedColor, fontColor, moonPurple, secondBackgroundColor, sunOrange)



---- MODEL ----


type alias Model =
    { theme : Theme
    }


init : ( Model, Cmd Msg )
init =
    ( { theme = Dark }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | ToggleTheme


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToggleTheme ->
            ( { model
                | theme =
                    if model.theme == Dark then
                        Light

                    else
                        Dark
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    layout
        [ Background.color <| backgroundColor model.theme
        , Font.family [ Font.typeface "Fira Sans", Font.sansSerif ]
        ]
    <|
        dashboard model


edges : { top : number, right : number, bottom : number, left : number }
edges =
    { top = 0, right = 0, bottom = 0, left = 0 }


darkToggle : Theme -> Element Msg
darkToggle theme =
    Input.button
        [ alignTop
        , alignRight
        , Border.rounded 15
        , paddingXY 10 10
        ]
        { onPress = Just ToggleTheme
        , label =
            if theme == Dark then
                el [ Font.color sunOrange ] (sun |> Element.html)

            else
                el [ Font.color moonPurple ] (moon |> Element.html)
        }


centralForm : Model -> Element msg
centralForm model =
    let
        header =
            el
                [ centerX
                , Region.heading 1
                , Font.size 24
                , Font.color <| fontColor model.theme
                ]
                (text "Alg Qs")

        inputForm =
            column
                [ padding 10
                , Background.color <| secondBackgroundColor model.theme
                , borderRadius
                , spacingXY 0 10
                , boxShadow model.theme
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
                        , Background.color <| ctaColor model.theme
                        , borderRadius
                        , padding 5
                        , alignLeft
                        , Element.focused
                            [ ctaFocusedColor ]
                        ]
                        { onPress = Nothing, label = el [ centerX ] <| text "Go" }
                    , Input.button
                        [ width fill
                        , Background.color <| buttonColor model.theme
                        , borderRadius
                        , padding 5
                        , alignRight
                        , Element.focused
                            [ buttonFocusedColor ]
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


answers : Model -> Element msg
answers model =
    column
        [ centerX
        , centerY
        , width <| px 400
        , padding 10
        , Background.color <| secondBackgroundColor model.theme
        , borderRadius
        , spacingXY 0 10
        , Font.color <| fontColor model.theme
        , boxShadow model.theme
        ]
        [ row []
            [ text "Q: ", text "What is three plus 17?" ]
        , row []
            [ text "A: ", text "Twenty" ]
        ]


footer : Model -> Element msg
footer model =
    row
        [ alignBottom
        , centerX
        , width fill
        , padding 20
        , Font.color <| fontColor model.theme
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


dashboard : Model -> Element Msg
dashboard model =
    column
        [ height fill
        , width fill
        ]
        [ darkToggle model.theme
        , centralForm model
        , answers model
        , footer model
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
