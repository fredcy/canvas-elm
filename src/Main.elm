module Main exposing (Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode
import RemoteData


type alias Flags =
    Json.Decode.Value


type alias Token =
    String


type alias Authentication =
    { access_token : Token
    , name : String
    }


type alias Model =
    { authentication : Result Json.Decode.Error Authentication
    , auth_providers : RemoteData.WebData (List AuthProvider)
    }


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
        authResult =
            Json.Decode.decodeValue flagsDecoder flags

        cmd =
            case authResult of
                Ok auth ->
                    requestAuthProviders auth.access_token

                Err _ ->
                    Cmd.none

        auth_providers =
            case authResult of
                Ok _ ->
                    RemoteData.Loading

                Err _ ->
                    RemoteData.NotAsked
    in
    ( { authentication = authResult, auth_providers = auth_providers }, cmd )


flagsDecoder : Json.Decode.Decoder Authentication
flagsDecoder =
    Json.Decode.map2 Authentication
        (Json.Decode.field "access_token" Json.Decode.string)
        (Json.Decode.at [ "user", "name" ] Json.Decode.string)


type Msg
    = NoOp
    | GotAuthProviders (RemoteData.WebData (List AuthProvider))


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        _ =
            Debug.log "msg" (Debug.toString msg)
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotAuthProviders r ->
            ( { model | auth_providers = r }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Simple Canvas app"
    , body =
        [ div []
            [ div [] [ text (Debug.toString model) ]
            ]
        , viewAuthProviders model.auth_providers
        ]
    }


type alias AuthProvider =
    { id : Int
    , auth_type : String
    , position : Int
    }


authProviderDecoder : Json.Decode.Decoder AuthProvider
authProviderDecoder =
    Json.Decode.map3 AuthProvider
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "auth_type" Json.Decode.string)
        (Json.Decode.field "position" Json.Decode.int)


proxyURL =
    "https://app.imsa.edu/connect/canvas/proxy"


requestAuthProviders : Token -> Cmd Msg
requestAuthProviders token =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ token)
            , Http.header "Token" token
            ]
        , url = proxyURL ++ "/api/v1/accounts/1/authentication_providers"
        , body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> GotAuthProviders) (Json.Decode.list authProviderDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }

viewAuthProviders : RemoteData.WebData (List AuthProvider) -> Html Msg
viewAuthProviders providersWD =
    div [] [ text (Debug.toString providersWD) ]
