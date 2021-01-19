module Types exposing (..)

import Browser
import Browser.Navigation as Nav
import Graphql.Http
import Json.Decode
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import UUID exposing (UUID)
import Url



{- All custom types currently live here.
   This includes the Model, and the Msgs.
-}


{-| TEA Model
-}
type alias Model =
    -- User's name. Can be changed at will.
    { name : String

    -- User's UUID. Used to uniquely identify users. Generated once per session.
    , uuid : Maybe String

    -- Content of the chat message currently being written.
    , currentContent : String

    -- List of all recieved chat messages.
    , chatMessages : RemoteData (Graphql.Http.Error Response) Response

    -- Are we connected to WebSockets?
    , subscriptionStatus : SubscriptionStatus

    -- Needed to enable navigation commands.
    , key : Nav.Key

    -- Current navigation route
    , route : Route
    }


{-| TEA Msgs
-}
type Msg
    = -- Got a result from the `messages` query
      GotChat (RemoteData (Graphql.Http.Error Response) Response)
      -- Send a new ChatMessage
    | SendMessage ChatMessage
      -- A ChatMessage was sent successfully
    | MessageSent (Result (Graphql.Http.Error ChatMessage) ChatMessage)
      -- Change the user's name
    | ChangeName String
      -- Change the current message content
    | ChangeContent String
      -- WebSockets connection status changed
    | NewSubscriptionStatus SubscriptionStatus ()
      -- Recieved a ChatMessage over WebSockets
    | MessageSubscriptionDataReceived Json.Decode.Value
      -- Recieved a UUID for the session
    | SeedResult UUID
      -- Clicked a link- in here just handle navigation
    | LinkClicked Browser.UrlRequest
      -- URL changed- in here just handle route parsing
    | UrlChanged Url.Url


{-| A single chat message.
-}
type alias ChatMessage =
    { content : String
    , author : String
    , authorUuid : String
    }


{-| Convenience helper- a list of ChatMessages.
Used as a response type in several places.
-}
type alias Response =
    List ChatMessage


{-| WebSockets connection status
-}
type SubscriptionStatus
    = NotConnected
    | Connected


{-| TEA Flags
Currently, none in use.
-}
type alias Flags =
    ()
