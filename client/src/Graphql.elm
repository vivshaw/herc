module Graphql exposing (..)

import ChatAPI.Mutation as Mutation
import ChatAPI.Object
import ChatAPI.Object.Message as Message exposing (author, content)
import ChatAPI.Query as Query
import ChatAPI.Subscription as Subscription
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import RemoteData
import Types exposing (..)



-- GraphQL Setup


{-| The GraphQL endpoint we plan to hit. Currently hard-coded.
-}
endpoint : String
endpoint =
    -- FIXME: pull from .env or something
    "https://herc-server.herokuapp.com/graphql"


{-| The sole SelectionSet.
Selects all fields from a ChatMessage.
I'm not doing anything fancy yet, so this is currently used for all queries.
-}
messageSelection : SelectionSet ChatMessage ChatAPI.Object.Message
messageSelection =
    SelectionSet.map3 ChatMessage
        Message.content
        Message.author
        Message.authorUuid


{-| The sole Query.
Gets all Messages.
-}
messagesQuery : SelectionSet Response RootQuery
messagesQuery =
    Query.messages messageSelection


{-| The sole Mutation.
Creates a single Message.
-}
sendMessageMutation : ChatMessage -> SelectionSet ChatMessage RootMutation
sendMessageMutation message =
    Mutation.sendMessage message messageSelection


{-| The sole Subscription.
Fires when new Messages are recieved.
-}
subscriptionDocument : SelectionSet ChatMessage RootSubscription
subscriptionDocument =
    Subscription.messageSent messageSelection



-- Execution helpers


{-| Executes a query, then passes the result as a GotChat msg.
-}
doQuery : SelectionSet Response RootQuery -> Cmd Msg
doQuery query =
    query
        |> Graphql.Http.queryRequest endpoint
        |> Graphql.Http.send (RemoteData.fromResult >> GotChat)


{-| Executes a mutation, then passes the result as a MessageSent msg.
-}
doMutation : SelectionSet ChatMessage RootMutation -> Cmd Msg
doMutation mutation =
    mutation
        |> Graphql.Http.mutationRequest endpoint
        |> Graphql.Http.send MessageSent
