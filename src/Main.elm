port module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import ExampleStatementGenerator exposing (ExampleStatement, generateStatement, prettyExampleStatement)
import Html.Events
import Icons exposing (github, moon, send, sun)
import Json.Decode as Decode
import Palette exposing (Theme, ThemeName(..), getTheme)
import Parser
import Random
import StatementParser exposing (StatementParserResult, StatementValue(..), getStatementErrorMessage, parseStatement, prettyNum, toNumeralEquation, toNumericAnswer)
import Url exposing (Url)
import Url.Builder as UrlBuilder
import Url.Parser as UrlParser
import Url.Parser.Query as QueryParser
import WrittenOutNumber exposing (writeOut)



---- MODEL ----


type alias InitFlags =
    { theme : String
    , height : Int
    , width : Int
    }


type alias Model =
    { device : Device
    , theme : Theme
    , key : Navigation.Key
    , question : String
    , answer : Maybe StatementParserResult
    }


init : InitFlags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialQuestion : String
        initialQuestion =
            questionFromQuery url

        initialAnswer =
            if initialQuestion == "" then
                Nothing

            else
                parseStatement initialQuestion
                    |> Just

        initialTheme =
            if flags.theme == "dark" then
                getTheme Dark

            else
                getTheme Light
    in
    ( { key = key
      , theme = initialTheme
      , question = initialQuestion
      , answer = initialAnswer
      , device = classifyDevice { height = flags.height, width = flags.width }
      }
    , Cmd.none
    )


port toggleTheme : String -> Cmd msg



---- UPDATE ----


type Msg
    = NoOp
    | ToggleTheme
    | UpdateQuestion String
    | SubmitQuestion
    | GenerateRandomQuestion
    | SubmitRandomQuestion ExampleStatement
    | UpdateQuestionFromPageChange Url


questionFromQuery : Url -> String
questionFromQuery url =
    case UrlParser.parse (UrlParser.query (QueryParser.string "q")) url of
        Just (Just question) ->
            question

        _ ->
            ""


pushNewQuestionUrl : Navigation.Key -> String -> Cmd msg
pushNewQuestionUrl key question =
    UrlBuilder.relative [] [ UrlBuilder.string "q" question ]
        |> Navigation.pushUrl key


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
                    if model.theme.name == Dark then
                        getTheme Light

                    else
                        getTheme Dark
              }
            , toggleTheme
                (if model.theme.name == Dark then
                    "light"

                 else
                    "dark"
                )
            )

        SubmitQuestion ->
            ( { model
                | question = String.trim model.question
                , answer = parseStatement model.question |> Just
              }
            , pushNewQuestionUrl model.key <| String.trim model.question
            )

        GenerateRandomQuestion ->
            ( model, Random.generate SubmitRandomQuestion generateStatement )

        SubmitRandomQuestion exampleQ ->
            let
                newQ =
                    prettyExampleStatement exampleQ
            in
            ( { model
                | question = newQ
                , answer = parseStatement newQ |> Just
              }
            , pushNewQuestionUrl model.key newQ
            )

        UpdateQuestionFromPageChange url ->
            let
                newQ =
                    questionFromQuery url
            in
            if newQ == "" then
                ( model, Cmd.none )

            else
                ( { model
                    | question = newQ
                    , answer = parseStatement newQ |> Just
                  }
                , Cmd.none
                )



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


answerText : Theme -> StatementParserResult -> Element msg
answerText theme answerObj =
    let
        errorMessage =
            el [ Font.color theme.errorTextColor ]
    in
    case answerObj of
        Err err ->
            errorMessage
                (err
                    |> getStatementErrorMessage
                    |> text
                )

        Ok answer ->
            let
                answerResult =
                    toNumericAnswer answer
            in
            case answerResult of
                Ok answerNum ->
                    case writeOut answerNum of
                        Ok writtenOutAnswer ->
                            writtenOutAnswer
                                ++ ". "
                                ++ "("
                                ++ prettyNum answerNum
                                ++ ")"
                                |> capitalize
                                |> text

                        _ ->
                            answerNum |> prettyNum |> capitalize |> text

                _ ->
                    text "Parse Error" |> errorMessage


view : Model -> Browser.Document Msg
view model =
    { title = "Algebra Qs"
    , body =
        [ layout
            [ Background.color model.theme.backgroundColor
            , Font.family [ Font.typeface "Fira Sans", Font.sansSerif ]
            ]
          <|
            dashboard model
        ]
    }


themeToggleButton : Theme -> Element Msg
themeToggleButton theme =
    Input.button
        [ alignTop
        , alignRight
        , Border.roundEach
            { topRight = 0
            , bottomRight = 0
            , bottomLeft = 25
            , topLeft = 0
            }
        , paddingXY 10 10
        , Element.focused []
        , Element.mouseOver [ Background.color <| rgba255 0 0 0 0.15 ]
        ]
        { onPress = Just ToggleTheme
        , label =
            el [ Font.color theme.themeToggleButtonColor ]
                (if theme.name == Dark then
                    Element.html sun

                 else
                    Element.html moon
                )
        }


centralForm : Model -> Element Msg
centralForm model =
    let
        header =
            el
                [ Region.heading 1
                , Font.size 24
                , Font.color model.theme.textColor
                ]
                (text "Alg Qs")

        buttonStyles =
            [ height fill
            , Border.rounded model.theme.borderRadius
            , padding 5
            , Font.size 16
            , Font.color model.theme.buttonTextColor
            , Border.shadow
                { offset = ( 0, 2 )
                , size = 0
                , blur = 1
                , color = rgba255 0 0 0 0.5
                }
            , Element.focused []
            ]

        submitEnabled =
            String.trim model.question /= ""

        inputForm =
            column
                [ Background.color model.theme.sectionBackgroundColor
                , Border.rounded model.theme.borderRadius
                , spacingXY 0 10
                , padding 10
                ]
                [ Input.multiline
                    [ Border.rounded model.theme.borderRadius
                    , onEnter SubmitQuestion
                    , Font.color model.theme.textColor
                    , Background.color model.theme.backgroundColor
                    ]
                    { text = model.question
                    , placeholder = Just (Input.placeholder [] <| text "Ask me a question")
                    , label = Input.labelHidden "What is your algebraic question?"
                    , onChange = UpdateQuestion
                    , spellcheck = True
                    }
                , row [ width fill, spacing 10 ]
                    [ Input.button
                        (List.concat
                            [ buttonStyles
                            , [ width <| fillPortion 4 ]
                            , if submitEnabled then
                                [ Background.color model.theme.ctaColor
                                , Element.mouseOver [ Background.color model.theme.ctaFocusedColor ]
                                , Element.mouseDown
                                    [ Border.shadow
                                        { offset = ( 0, 0 )
                                        , size = 0
                                        , blur = 0
                                        , color = rgb255 0 0 0
                                        }
                                    ]
                                ]

                              else
                                [ Background.color model.theme.disabledBackgroundColor
                                , Border.shadow
                                    { offset = ( 0, 0 )
                                    , size = 0
                                    , blur = 0
                                    , color = rgb255 0 0 0
                                    }
                                , Font.color model.theme.disabledFontColor
                                ]
                            ]
                        )
                        { onPress =
                            if submitEnabled then
                                Just SubmitQuestion

                            else
                                Nothing
                        , label =
                            row [ centerX ]
                                [ el [] (Element.html send)
                                , el [] <| text "Go"
                                ]
                        }
                    , Input.button
                        (List.concat
                            [ buttonStyles
                            , [ width <| fillPortion 1
                              , Background.color model.theme.buttonColor
                              , Element.focused []
                              , Element.mouseOver [ Background.color model.theme.buttonFocusedColor ]
                              , Element.mouseDown
                                    [ Border.shadow
                                        { offset = ( 0, 0 )
                                        , size = 0
                                        , blur = 0
                                        , color = rgb255 0 0 0
                                        }
                                    ]
                              ]
                            ]
                        )
                        { onPress = Just GenerateRandomQuestion
                        , label = el [ centerX ] <| Element.html Icons.refreshCw
                        }
                    ]
                ]

        formPositioning =
            case model.device.class of
                Phone ->
                    [ width fill
                    , spacing 15
                    , paddingEach { top = 20, right = 0, bottom = 0, left = 0 }
                    ]

                _ ->
                    [ centerY
                    , width fill
                    , spacing 10
                    ]
    in
    textColumn formPositioning
        [ header
        , inputForm
        ]


numeralEquationText : StatementParserResult -> Maybe (Element msg)
numeralEquationText val =
    case val of
        Ok stmt ->
            stmt
                |> toNumeralEquation
                |> text
                |> List.singleton
                |> paragraph [ Font.underline ]
                |> Just

        _ ->
            Nothing


answers : Theme -> StatementParserResult -> Element msg
answers theme answer =
    let
        equationText =
            numeralEquationText answer
    in
    column
        [ centerX
        , padding 10
        , width fill
        , Background.color theme.sectionBackgroundColor
        , Border.rounded theme.borderRadius
        , spacingXY 0 10
        , Font.color theme.textColor
        ]
        (List.append (equationText |> Maybe.map List.singleton |> Maybe.withDefault [])
            [ paragraph []
                [ answerText theme answer ]
            ]
        )


capitalize : String -> String
capitalize str =
    let
        firstLetter =
            String.left 1 str |> String.toUpper
    in
    firstLetter ++ String.dropLeft 1 str


footer : Model -> Element msg
footer model =
    row
        [ alignBottom
        , width fill
        , padding 20
        , Font.color model.theme.textColor
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
    let
        contentPositioning =
            case model.device.class of
                Phone ->
                    [ width fill
                    , paddingEach { top = 0, right = 15, bottom = 40, left = 15 }
                    , spacing 15
                    ]

                _ ->
                    [ width <| px 400
                    , centerX
                    , paddingXY 0 60
                    , spacing 10
                    ]

        answerPanel =
            case model.answer of
                Nothing ->
                    text ""

                Just answer ->
                    answers model.theme answer
    in
    column
        [ height fill
        , width fill
        ]
        [ themeToggleButton model.theme
        , textColumn contentPositioning
            [ centralForm model
            , answerPanel
            ]
        , footer model
        ]



---- PROGRAM ----


main : Program InitFlags Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UpdateQuestionFromPageChange
        , onUrlRequest = always NoOp
        }
