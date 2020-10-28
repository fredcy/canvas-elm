module Main exposing (Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode


type alias Flags =
    Json.Decode.Value


type alias Token =
    String


type alias Authentication =
    { access_token : Token
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


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            Json.Decode.decodeValue flagsDecoder flags

        cmd =
            case model of
                Ok auth ->
                    requestAuthProviders auth.access_token

                Err _ ->
                    Cmd.none
    in
    ( model, cmd )


flagsDecoder : Json.Decode.Decoder Authentication
flagsDecoder =
    Json.Decode.map2 Authentication
        (Json.Decode.field "access_token" Json.Decode.string)
        (Json.Decode.at [ "user", "name" ] Json.Decode.string)


type Msg
    = NoOp
    | GotAuthProviders (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotAuthProviders r ->
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


requestAuthProviders : Token -> Cmd Msg
requestAuthProviders token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = "https://cors-anywhere.herokuapp.com/https://imsa.instructure.com/api/vi/accounts/1/authentication_providers"
        , body = Http.emptyBody
        , expect = Http.expectString GotAuthProviders
        , timeout = Nothing
        , tracker = Nothing
        }
