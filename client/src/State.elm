port module State exposing (..)

import Browser
import Browser.Navigation as Nav
import Graphql exposing (..)
import Graphql.Document as Document
import Json.Decode
import Random
import RemoteData
import Route
import Types exposing (..)
import UUID
import Url



{- State lives here! `init`, `update`, and `subscription` are all handled here,
   as are Ports. See `Types.elm` for documentation on the Model and Msgs.
-}


{-| TEA Init
-}
init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { name = "Anonymous User"
      , currentContent = ""
      , uuid = Nothing
      , chatMessages = RemoteData.Loading
      , subscriptionStatus = NotConnected
      , key = key
      , route = Route.parseUrl url
      }
    , Cmd.batch
        -- On init, load chat messages...
        [ doQuery messagesQuery

        -- connect to WebSockets...
        , createSubscriptionToMessages (subscriptionDocument |> Document.serializeSubscription)

        -- and pick a random UUID for the session.
        , Random.generate SeedResult UUID.generator
        ]
    )


{-| TEA Update
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SeedResult uuid ->
            ( { model | uuid = Just (UUID.toString uuid) }, Cmd.none )

        GotChat response ->
            ( { model | chatMessages = response }, Cmd.none )

        ChangeName newName ->
            ( { model | name = newName }, Cmd.none )

        ChangeContent newContent ->
            ( { model | currentContent = newContent }, Cmd.none )

        SendMessage message ->
            -- Fire off the mutation and clear the content!
            ( { model | currentContent = "" }
            , doMutation (sendMessageMutation message)
            )

        MessageSent _ ->
            -- Currently, do nothing when a message sends,
            -- as it will show up in the Subscription.
            -- FIXME: could look at offline-first and cache this somewhere.
            ( model, Cmd.none )

        NewSubscriptionStatus newStatus () ->
            ( { model | subscriptionStatus = newStatus }, Cmd.none )

        MessageSubscriptionDataReceived newData ->
            case Json.Decode.decodeValue (subscriptionDocument |> Document.decoder) newData of
                Ok newMessage ->
                    -- JSON was well formed
                    case model.chatMessages of
                        -- Need to check if we have already loaded chat messages.
                        -- If not, we shouldn't be listening on a WebSocket yet!
                        RemoteData.Success messages ->
                            -- Messages are loaded!
                            -- Let's concatenate the new message to our chat state.
                            ( { model | chatMessages = RemoteData.Success (messages ++ [ newMessage ]) }, Cmd.none )

                        _ ->
                            -- Messages are not yet loaded. Do nothing.
                            ( model, Cmd.none )

                Err _ ->
                    -- JSON was not well formed. Currently fails silently.
                    -- FIXME: Better error handling here
                    ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    -- use Elm push state for internal links,
                    -- to make it a single page app.
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    -- Use regular links for external links.
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = Route.parseUrl url }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


{-| TEA Subscriptions
-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ gotMessageSubscriptionData MessageSubscriptionDataReceived
        , socketStatusConnected (NewSubscriptionStatus Connected)
        ]



-- Ports


{-| Calls into JS to open a WebSockets port with Apollo Client. Called in `init`.
-}
port createSubscriptionToMessages : String -> Cmd msg


{-| Subscription that is called when WebSockets connection is successful.
-}
port socketStatusConnected : (() -> msg) -> Sub msg


{-| Subscription that is called every time the GraphQL Subscription recieves data.
-}
port gotMessageSubscriptionData : (Json.Decode.Value -> msg) -> Sub msg
