module Main exposing (Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode


type alias Flags =
    Json.Decode.Value


type alias Model =
    String


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


init flags =
    ( "initialized", Cmd.none )


type Msg
    = NoOp


update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view model =
    { title = "Simple Canvas app"
    , body =
        [ div []
            [ div [] [ text model ]
            ]
        ]
    }
