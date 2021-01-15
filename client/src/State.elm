port module State exposing (..)

import Api exposing (..)
import Browser
import Browser.Navigation as Nav
import Graphql.Document as Document
import Json.Decode
import Random
import RemoteData
import Route
import Types exposing (..)
import UUID
import Url


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { name = "Anonymous User"
      , currentComment = ""
      , chatMessages = RemoteData.Loading
      , subscriptionStatus = NotConnected
      , uuid = Nothing
      , key = key
      , route = Route.parseUrl url
      }
    , Cmd.batch
        [ makeRequest
        , createSubscriptionToMessages (subscriptionDocument |> Document.serializeSubscription)
        , Random.generate SeedResult UUID.generator
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SeedResult uuid ->
            ( { model | uuid = Just (UUID.toString uuid) }, Cmd.none )

        GotChat response ->
            ( { model | chatMessages = response }, Cmd.none )

        ChangeName newName ->
            ( { model | name = newName }, Cmd.none )

        ChangeComment newComment ->
            ( { model | currentComment = newComment }, Cmd.none )

        SendMessage message ->
            ( { model | currentComment = "" }
            , doMutation (sendChatMessage message)
            )

        SentMessage _ ->
            ( model, Cmd.none )

        NewSubscriptionStatus newStatus () ->
            ( { model | subscriptionStatus = newStatus }, Cmd.none )

        MessageSubscriptionDataReceived newData ->
            case Json.Decode.decodeValue (subscriptionDocument |> Document.decoder) newData of
                Ok chatMessage ->
                    case model.chatMessages of
                        RemoteData.Success messages ->
                            ( { model | chatMessages = RemoteData.Success (messages ++ [ chatMessage ]) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = Route.parseUrl url }
            , Cmd.none
            )


port createSubscriptionToMessages : String -> Cmd msg


port socketStatusConnected : (() -> msg) -> Sub msg


port gotMessageSubscriptionData : (Json.Decode.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ gotMessageSubscriptionData MessageSubscriptionDataReceived
        , socketStatusConnected (NewSubscriptionStatus Connected)
        ]
