module Main exposing (Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode


type alias Flags =
    Json.Decode.Value


type alias Authentication =
    { access_token : String
    , name : String
    }


type alias Model =
    Result Json.Decode.Error Authentication


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


init : Flags -> ( Model, Cmd msg )
init flags =
    ( Json.Decode.decodeValue flagsDecoder flags, Cmd.none )


flagsDecoder : Json.Decode.Decoder Authentication
flagsDecoder =
    Json.Decode.map2 Authentication
        (Json.Decode.field "access_token" Json.Decode.string)
        (Json.Decode.at ["user", "name"] Json.Decode.string)


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Simple Canvas app"
    , body =
        [ div []
            [ div [] [ text (Debug.toString model) ]
            ]
        ]
    }
