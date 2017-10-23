module Main exposing (..)

import Random exposing (Seed, generate)
import Random.List exposing (shuffle)
import Random.Extra exposing (sample)

import String exposing (toInt)
import Result exposing (toMaybe)
import Html exposing (button, Html, text, div, img, input)
import Html.Events exposing (onClick, onInput)

import Svg exposing (..)
import Svg.Attributes exposing (..)

---- MODEL ----

dotSize = 10
dotGap = 5
scaleFactor = 2 * ( dotSize + dotGap )

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
        div []
            [ div []
                [ Html.text "min"
                , input [ onInput ChangeMin ] []
                ]
            , div []
                [ Html.text "max"
                , input [ onInput ChangeMax ] []
                ]
            , div []
                [ button [ onClick Shuffle ] [ Html.text "shuffle" ]
                ]
            , div [] <| [ partition positions ]
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
        unitPositions  = cartesian (List.range 1 10) (List.range 1 10)
    in
        List.map (scale scaleFactor) unitPositions

dot : Position -> Svg Msg
dot position = circle
    [ r <| toString dotSize
    , cx <| toString position.x
    , cy <| toString position.y
    ]
    []

type alias Position =
    { x: Int
    , y: Int
    }

partition : List Position -> Html Msg
partition positions =
    svg
        [ width "1000", height "1000", viewBox "0 0 1000 1000" ]
        ( List.map dot positions )

---- PROGRAM ----

main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
