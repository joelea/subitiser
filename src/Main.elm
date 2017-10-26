module Main exposing (..)

import Random exposing (Seed, generate)
import Random.List exposing (shuffle)
import Random.Extra exposing (sample)

import Color

import String exposing (toInt)
import Result exposing (toMaybe)
import Html exposing (button, Html, text, div, img, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, placeholder)

import Collage exposing (..)
import Element exposing (..)

---- MODEL ----

dotSize = 30
dotGap = 5
scaleFactor = dotGap + (2 * dotSize)
gridSize = 4

type alias Model =
    { min: Maybe Int
    , max: Maybe Int
    , shuffledPositions: List Position
    , value: Int
    }


init : ( Model, Cmd Msg )
init =
    ( { min = Nothing, max = Nothing, shuffledPositions = possiblePositions, value = 0 }, Cmd.none )

---- UPDATE ----

type Msg
    = NoOp
    | ChangeMin String
    | ChangeMax String
    | ChangeValue (Maybe Int)
    | ShuffledPositions (List Position)
    | Shuffle

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeMin newValue ->
            let
                possibleValue = toMaybe <| toInt newValue
                min = Maybe.withDefault 0 model.min
                max = Maybe.withDefault 0 model.max
            in
                ( { model | min = possibleValue }
                , generate ChangeValue (sample ( List.range min max ) )
                )
        ChangeMax newValue ->
            let
                possibleValue = toMaybe <| toInt newValue
                min = Maybe.withDefault 0 model.min
                max = Maybe.withDefault 0 model.max
            in
                ( { model | max = possibleValue }
                , generate ChangeValue (sample ( List.range min max ) )
                )
        ChangeValue newValue ->
            ( { model | value = Maybe.withDefault 0 newValue }
            , generate ShuffledPositions (shuffle possiblePositions)
            )
        Shuffle ->
            let
                min = Maybe.withDefault 0 model.min
                max = Maybe.withDefault 0 model.max
            in
                ( model
                , generate ChangeValue (sample ( List.range min max ) )
                )
        ShuffledPositions positions ->
            ( { model | shuffledPositions = positions }
            , Cmd.none
            )
        default ->
            ( model, Cmd.none )

---- VIEW ----

view : Model -> Html Msg
view model =
    let
        positions = List.take model.value model.shuffledPositions
    in
        div [ class "app" ]
            [
            div [ class "controls" ]
                [ div [ class "input-section" ]
                    [ div [] [ Html.text "Give me a number between" ]
                    , div [] [ input [ onInput ChangeMin, placeholder "min" ] [] ]
                    , div [] [ Html.text "and" ]
                    , div [] [ input [ onInput ChangeMax, placeholder "max" ] [] ]
                    ]
                , div []
                    [ button [ class "shuffle-button", onClick Shuffle ] [ Html.text "shuffle" ]
                    ]
                ]
            , div [ class "dots" ] <| [ partition positions ]
            ]


scale : Int -> Position -> Position
scale size pos = Position (size * pos.x) (size * pos.y)

cartesian : List Int -> List Int -> List Position
cartesian xs ys =
  List.concatMap
    ( \x -> List.map ( \y -> Position x y ) ys )
    xs

possiblePositions =
    let
        unitPositions  = cartesian (List.range -gridSize gridSize) (List.range -gridSize gridSize)
    in
        List.map (scale scaleFactor) unitPositions

dot : Position -> Form
dot position = circle dotSize
    |> filled Color.black
    |> translate position

type alias Position =
    { x: Int
    , y: Int
    }

translate : Position -> Form -> Form
translate position form = move (toFloat position.x, toFloat position.y) form

partition : List Position -> Html Msg
partition positions =
    let
        elements = List.map dot positions
        size = (2 * (gridSize + 1) ) * scaleFactor
        dots = collage size size elements
    in
        toHtml dots

---- PROGRAM ----

main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
