module Main exposing (Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import RemoteData exposing (RemoteData(..), WebData, fromResult)


type alias Flags =
    D.Value


type alias Token =
    String


type alias Authentication =
    { access_token : Token
    , name : String
    }


type AuthState
    = Authenticated Authentication
    | Unauthenticated
    | AuthError D.Error


type alias Model =
    { authentication : AuthState
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
            case D.decodeValue flagsDecoder flags of
                Ok authInfo ->
                    Authenticated authInfo

                Err error ->
                    AuthError error

        ( auth_providers, cmd ) =
            case authResult of
                Authenticated authInfo ->
                    ( Loading, requestAuthProviders authInfo.access_token )

                _ ->
                    ( NotAsked, Cmd.none )
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
    case Debug.log "msg" msg of
        NoOp ->
            ( model, Cmd.none )

        GotAuthProviders r ->
            let
                _ =
                    checkAuthorization r
            in
            ( { model | auth_providers = r }, Cmd.none )


checkAuthorization responseWD =
    case responseWD of
        Failure (Http.BadStatus code) ->
            let
                _ =
                    Debug.log "HTTP response code" code
            in
            Cmd.none

        _ ->
            let
                _ =
                    Debug.log "other response" responseWD
            in
            Cmd.none


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
    "http://localhost:8081"



--    "https://app.imsa.edu/connect/canvas/proxy"


requestAuthProviders : Token -> Cmd Msg
requestAuthProviders token =
    Http.request
        { method = "GET"
        , headers =
            [{- Http.header "Authorization" ("Bearer " ++ token)
                -            , Http.header "Token" token
             -}
            ]
        , url = proxyURL ++ "/api/v1/accounts/1/authentication_providers"
        , body = Http.emptyBody
        , expect = Http.expectJson (fromResult >> GotAuthProviders) (D.list authProviderDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


viewAuthProviders : WebData (List AuthProvider) -> Html Msg
viewAuthProviders providersWD =
    div []
        [ Html.h2 [] [ text "Auth Providers" ]
        , div [] [ text (Debug.toString providersWD) ]
        , case providersWD of
            Success providers ->
                viewAuthProviderList providers

            _ ->
                div [] [ text (Debug.toString providersWD) ]
        ]


viewAuthProviderList : List AuthProvider -> Html Msg
viewAuthProviderList providers =
    let
        viewProvider p =
            Html.tr []
                [ Html.td [] [ text p.auth_type ]
                , Html.td [] [ text (String.fromInt p.id) ]
                ]

        viewHeader n =
            Html.th [] [ text n ]
    in
    Html.table []
        [ Html.thead [] [ Html.tr [] (List.map viewHeader [ "name", "id" ]) ]
        , Html.tbody [] (List.map viewProvider providers)
        ]
