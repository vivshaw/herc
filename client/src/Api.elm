module Api exposing (..)

import ChatAPI.Mutation as Mutation
import ChatAPI.Object exposing (Message)
import ChatAPI.Object.Message as Message exposing (author, content)
import ChatAPI.Query as Query
import ChatAPI.Scalar
import ChatAPI.ScalarCodecs
import ChatAPI.Subscription as Subscription
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import RemoteData exposing (RemoteData)
import Types exposing (..)



-- GraphQL Setup


endpoint : String
endpoint =
    -- FIXME: pull from .env or something
    "https://herc-server.herokuapp.com/graphql"


query : SelectionSet Response RootQuery
query =
    Query.messages messageSelection


messageSelection : SelectionSet ChatMessage ChatAPI.Object.Message
messageSelection =
    SelectionSet.map3 ChatMessage
        Message.content
        Message.author
        Message.authorUuid


makeRequest : Cmd Msg
makeRequest =
    query
        |> Graphql.Http.queryRequest endpoint
        |> Graphql.Http.send (RemoteData.fromResult >> GotChat)


sendChatMessage : ChatMessage -> SelectionSet ChatMessage RootMutation
sendChatMessage message =
    Mutation.sendMessage message messageSelection


doMutation : SelectionSet ChatMessage RootMutation -> Cmd Msg
doMutation mutation =
    mutation
        |> Graphql.Http.mutationRequest endpoint
        |> Graphql.Http.send SentMessage


subscriptionDocument : SelectionSet ChatMessage RootSubscription
subscriptionDocument =
    Subscription.messageSent messageSelection
