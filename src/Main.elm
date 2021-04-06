port module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import ExampleStatementGenerator exposing (ExampleStatement, generateStatement, prettyExampleStatement)
import Html exposing (Html)
import Html.Events
import Icons exposing (github, moon, sun)
import Json.Decode as Decode
import Palette exposing (Theme(..), backgroundColor, borderRadius, boxShadow, buttonColor, buttonFocusedColor, ctaColor, ctaFocusedColor, fontColor, moonPurple, secondBackgroundColor, sunOrange)
import Random
import StatementParser exposing (answer, toNumeralEquation)
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
    , answer : Maybe (Result String String)
    }


init : InitFlags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialQuestion : String
        initialQuestion =
            questionFromQuery url

        initialAnswer =
            if initialQuestion == "" then
                Nothing

            else
                parseAnswer initialQuestion
                    |> Just

        initialTheme =
            if flags.theme == "dark" then
                Dark

            else
                Light
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
    | UpdateQuestionFromPageChange Url.Url


questionFromQuery : Url.Url -> String
questionFromQuery url =
    case UrlParser.parse (UrlParser.query (QueryParser.string "q")) url of
        Just (Just question) ->
            question

        _ ->
            ""


parseAnswer : String -> Result String String
parseAnswer q =
    case answer q of
        Just num ->
            case writeOut num of
                Ok ans ->
                    Ok ans

                _ ->
                    Err "Answer too large to display"

        _ ->
            Err "Parse error"


pushNewQuestion : Navigation.Key -> String -> Cmd msg
pushNewQuestion key question =
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
                    if model.theme == Dark then
                        Light

                    else
                        Dark
              }
            , toggleTheme
                (if model.theme == Dark then
                    "light"

                 else
                    "dark"
                )
            )

        SubmitQuestion ->
            ( { model | answer = Just <| parseAnswer <| model.question }
            , pushNewQuestion model.key model.question
            )

        GenerateRandomQuestion ->
            ( model, Random.generate SubmitRandomQuestion generateStatement )

        SubmitRandomQuestion exampleQ ->
            let
                newQ =
                    prettyExampleStatement exampleQ
            in
            ( { model | question = newQ, answer = Just <| parseAnswer <| newQ }
            , pushNewQuestion model.key newQ
            )

        UpdateQuestionFromPageChange url ->
            let
                newQ =
                    questionFromQuery url
            in
            if newQ == "" then
                ( model, Cmd.none )

            else
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


answerText : Theme -> Maybe (Result String String) -> Element msg
answerText theme answerObj =
    case answerObj of
        Nothing ->
            text "Answer Not Found"

        Just (Err err) ->
            el [ theme |> Palette.buttonColor |> Font.color ] (text err)

        Just (Ok answer) ->
            answer ++ "." |> capitalize |> text


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        _ ->
            False


view : Model -> Browser.Document Msg
view model =
    { title = "Algebra Qs"
    , body =
        [ layout
            [ Background.color <| backgroundColor model.theme
            , Font.family [ Font.typeface "Fira Sans", Font.sansSerif ]
            ]
          <|
            dashboard model
        ]
    }


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
                [ Region.heading 1
                , Font.size 24
                , Font.color <| fontColor model.theme
                ]
                (text "Alg Qs")

        buttonStyles =
            [ width fill
            , borderRadius
            , padding 5
            ]

        inputForm =
            column
                [ Background.color <| secondBackgroundColor model.theme
                , borderRadius
                , spacingXY 0 10
                , padding 10
                ]
                [ Input.multiline
                    [ borderRadius
                    , onEnter SubmitQuestion
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
                            , [ Background.color <| ctaColor model.theme
                              , Element.focused
                                    [ ctaFocusedColor ]
                              ]
                            ]
                        )
                        { onPress = Just SubmitQuestion, label = el [ centerX ] <| text "Go" }
                    , Input.button
                        (List.concat
                            [ buttonStyles
                            , [ Background.color <| buttonColor model.theme
                              , Element.focused
                                    [ buttonFocusedColor ]
                              ]
                            ]
                        )
                        { onPress = Just GenerateRandomQuestion, label = el [ centerX ] <| text "Random" }
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


answers : Model -> Element msg
answers model =
    column
        [ centerX
        , padding 10
        , width fill
        , Background.color <| secondBackgroundColor model.theme
        , borderRadius
        , spacingXY 0 10
        , Font.color <| fontColor model.theme
        ]
        [ paragraph [ Font.underline ]
            [ model.question
                |> toNumeralEquation
                |> Maybe.withDefault model.question
                |> text
            ]
        , paragraph []
            [ answerText model.theme model.answer ]
        ]


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
    in
    column
        [ height fill
        , width fill
        ]
        [ darkToggle model.theme
        , textColumn contentPositioning
            [ centralForm model
            , if isJust model.answer then
                answers model

              else
                text ""
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
