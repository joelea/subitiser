module Main exposing (..)

import String exposing (toInt)
import Result exposing (toMaybe)
import Html exposing (Html, text, div, img, input)
import Html.Events exposing (onInput)


---- MODEL ----

type alias Model =
    {  value: Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    ( { value = Nothing }, Cmd.none )

---- UPDATE ----

type Msg
    = NoOp
    | Change String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newValue ->
            let
                possibleValue = toMaybe <| toInt newValue
            in
                ( { model | value = possibleValue }, Cmd.none )
        default ->
            ( model, Cmd.none )

---- VIEW ----

view : Model -> Html Msg
view model =
    let
        value = Maybe.withDefault "Please enter a number" possibleValue
        possibleValue = Maybe.map (\x -> toString x) model.value
    in
        div []
            [ input [ onInput Change ] []
            , text value
            ]

---- PROGRAM ----

main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
