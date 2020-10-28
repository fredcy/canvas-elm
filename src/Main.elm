module Main exposing (Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import RemoteData exposing (WebData, RemoteData(..), fromResult)


type alias Flags =
    D.Value


type alias Token =
    String


type alias Authentication =
    { access_token : Token
    , name : String
    }


type alias Model =
    { authentication : Result D.Error Authentication
    , auth_providers : WebData (List AuthProvider)
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
            D.decodeValue flagsDecoder flags

        cmd =
            case authResult of
                Ok auth ->
                    requestAuthProviders auth.access_token

                Err _ ->
                    Cmd.none

        auth_providers =
            case authResult of
                Ok _ ->
                    Loading

                Err _ ->
                    NotAsked
    in
    ( { authentication = authResult, auth_providers = auth_providers }, cmd )


flagsDecoder : D.Decoder Authentication
flagsDecoder =
    D.map2 Authentication
        (D.field "access_token" D.string)
        (D.at [ "user", "name" ] D.string)


type Msg
    = NoOp
    | GotAuthProviders (WebData (List AuthProvider))


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


authProviderDecoder : D.Decoder AuthProvider
authProviderDecoder =
    D.map3 AuthProvider
        (D.field "id" D.int)
        (D.field "auth_type" D.string)
        (D.field "position" D.int)


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
        , expect = Http.expectJson (fromResult >> GotAuthProviders) (D.list authProviderDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }

viewAuthProviders : WebData (List AuthProvider) -> Html Msg
viewAuthProviders providersWD =
    div [] [ text (Debug.toString providersWD) ]
