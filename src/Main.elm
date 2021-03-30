module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import ExampleStatementGenerator exposing (ExampleStatement, generateStatement, prettyExampleStatement)
import Html exposing (Html, a)
import Html.Events
import Icons exposing (github, moon, sun)
import Json.Decode as Decode
import Palette exposing (Theme(..), backgroundColor, borderRadius, boxShadow, buttonColor, buttonFocusedColor, ctaColor, ctaFocusedColor, fontColor, moonPurple, secondBackgroundColor, sunOrange)
import Random
import StatementParser exposing (answer)
import WrittenOutNumber exposing (writeOut)



---- MODEL ----


type alias Model =
    { theme : Theme
    , question : String
    , answer : Maybe (Result String String)
    }


init : ( Model, Cmd Msg )
init =
    ( { theme = Dark
      , question = ""
      , answer = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | ToggleTheme
    | UpdateQuestion String
    | SubmitQuestion
    | GenerateRandomQuestion
    | SubmitRandomQuestion ExampleStatement


parseAnswer : String -> Result String String
parseAnswer q =
    case answer q of
        Just num ->
            case writeOut num of
                Ok ans ->
                    Ok ans

                _ ->
                    Err "writting out errpr"

        _ ->
            Err "parse error"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateQuestion newQ ->
            ( { model
                | question = newQ
                , answer = Nothing
              }
            , Cmd.none
            )

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

        SubmitQuestion ->
            ( { model | answer = Just <| parseAnswer <| model.question }, Cmd.none )

        GenerateRandomQuestion ->
            -- ( model, Random.generate generateStatement SubmitRandomQuestion )
            ( model, Cmd.none )

        SubmitRandomQuestion exampleQ ->
            let
                newQ =
                    prettyExampleStatement exampleQ
            in
            ( { model | question = newQ, answer = Just <| parseAnswer <| newQ }, Cmd.none )



---- VIEW ----


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


answerText : Maybe (Result String String) -> String
answerText answerObj =
    case answerObj of
        Nothing ->
            ""

        Just (Err err) ->
            "Err- " ++ err

        Just (Ok answer) ->
            answer


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        _ ->
            False


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


centralForm : Model -> Element Msg
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
                [ Input.text
                    [ borderRadius
                    , onEnter SubmitQuestion
                    ]
                    { text = model.question
                    , placeholder = Just (Input.placeholder [] <| text "Ask me a question")
                    , label = Input.labelHidden "What is your algebraic question?"
                    , onChange = UpdateQuestion
                    }
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
                        { onPress = Just SubmitQuestion, label = el [ centerX ] <| text "Go" }
                    , Input.button
                        [ width fill
                        , Background.color <| buttonColor model.theme
                        , borderRadius
                        , padding 5
                        , alignRight
                        , Element.focused
                            [ buttonFocusedColor ]
                        ]
                        { onPress = Just GenerateRandomQuestion, label = el [ centerX ] <| text "Random" }
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
            [ text "Q: ", text model.question ]
        , row []
            [ text "A: ", text <| answerText model.answer ]
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
        , if isJust model.answer then
            answers model

          else
            text ""
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
