module Types exposing (..)

import Browser
import Browser.Navigation as Nav
import Graphql.Http
import Json.Decode
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import UUID exposing (UUID)
import Url


type Msg
    = GotChat (RemoteData (Graphql.Http.Error Response) Response)
    | SendMessage ChatMessage
    | MessageSent (Result (Graphql.Http.Error ChatMessage) ChatMessage)
    | ChangeName String
    | ChangeContent String
    | NewSubscriptionStatus SubscriptionStatus ()
    | MessageSubscriptionDataReceived Json.Decode.Value
    | SeedResult UUID
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


type alias ChatMessage =
    { content : String
    , author : String
    , authorUuid : String
    }


type alias Response =
    List ChatMessage


type SubscriptionStatus
    = NotConnected
    | Connected


type alias Model =
    { name : String
    , currentContent : String
    , chatMessages : RemoteData (Graphql.Http.Error Response) Response
    , subscriptionStatus : SubscriptionStatus
    , uuid : Maybe String
    , key : Nav.Key
    , route : Route
    }


type alias Flags =
    ()
