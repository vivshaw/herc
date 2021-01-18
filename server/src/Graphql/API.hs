{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Graphql.API
  ( morpheusApp,
    APIEvent,
    getApi,
  )
where

import Control.Monad.Except
import Data.Morpheus
  ( App,
    deriveApp,
  )
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Subscriptions
  ( Event (..),
    ServerApp,
    httpPubApp,
    webSocketsApp,
  )
import Data.Morpheus.Types
  ( GQLRequest,
    GQLResponse,
    MUTATION,
    QUERY,
    Resolver,
    ResolverM,
    ResolverQ,
    ResolverS,
    RootResolver (..),
    SubscriptionField,
    publish,
    subscribe,
  )
import Graphql.Types (APIEvent, Channel (..))
import Import hiding (App, Query, publish, (.))

-- I am using schema-first GraphQL, so I must first import the schema.
-- FIXME: Would code-first be more elegant here?
importGQLDocument "schema.gql"

-- Maps a single MessagePersist to a Morpheus GraphQL Message.
-- FIXME: Probably a more elegant way to do this.
toGqlMessage :: Monad m => MessagePersist -> Message m
toGqlMessage msg =
  Message
    { content = pure $ messagePersistContent msg,
      author = pure $ messagePersistAuthor msg,
      authorUuid = pure $ messagePersistAuthorUuid msg
    }

-- Our root resolver will unite all our sub-resolvers,
-- and handle all GraphQL requests recieved by our backend.
rootResolver :: RootResolver Handler APIEvent Query Mutation Subscription
rootResolver =
  RootResolver
    { queryResolver = Query {messages},
      mutationResolver = Mutation {sendMessage},
      subscriptionResolver = Subscription {messageSent}
    }
  where
    -- Query resolver.
    -- Simply returns a list of all Messages in the DB.
    messages :: ResolverQ APIEvent Handler [Message (Resolver QUERY APIEvent Handler)]
    messages = lift getAllMessagesDB

    -- Mutation resolver.
    -- Inserts a single Message to the DB, and returns it.
    sendMessage :: SendMessageArgs -> ResolverM APIEvent Handler Message
    sendMessage SendMessageArgs {content, author, authorUuid} = do
      publish [Event [Channel] msg]
      lift (insertMessageDB msg)
      where
        msg =
          MessagePersist
            { messagePersistContent = content,
              messagePersistAuthor = author,
              messagePersistAuthorUuid = authorUuid
            }

    -- Subscription resolver.
    -- Is called only by `sendMessage`, and simply echoes back all Messages
    -- that are sent, to enable real-time chatting.
    -- FIXME: Pretty sure this can be simplified.
    messageSent :: SubscriptionField (ResolverS APIEvent Handler Message)
    messageSent = subscribe Channel $ do
      pure $ \(Event _ msg) ->
        pure $ toGqlMessage msg

-- Handler to insert a single Message into the database.
-- Returns the inserted Message.
insertMessageDB :: MessagePersist -> Handler (Message (Resolver MUTATION APIEvent Handler))
insertMessageDB msg = do
  insertedMsg <- runDB $ insertEntity msg
  pure $ toGqlMessage $ entityVal insertedMsg

-- Handler to return a list of all Messages in our database,
-- then fmap them to the appropriate format for our Query resolver.
-- Returns a List of Messages, in ascending order by ID.
getAllMessagesDB :: Handler [Message (Resolver QUERY APIEvent Handler)]
getAllMessagesDB = do
  allMessages <- runDB $ selectList [] [Asc MessagePersistId]
  pure $ toGqlMessage . entityVal <$> allMessages

-- Derive our App. We will only work with this inside `getApi`.
morpheusApp :: App APIEvent Handler
morpheusApp = deriveApp rootResolver

-- We define our GraphQL API in this centralized location,
-- so that the GraphQL app and WebSockets app share the same `publish` instance.
-- Returns a tuple:
--
-- ( graphqlApi :: GQLRequest -> Handler GQLResponse
-- , wsApp :: ServerApp
-- , publish :: APIEvent -> Handler ())
--
-- This will be called in Application to provide the GraphQL services to the app.
-- FIXME: Syntax could possibly be cleaned up with a record type.
getApi :: Handler (GQLRequest -> Handler GQLResponse, ServerApp, APIEvent -> Handler ())
getApi = do
  (wsApp, publish') <- webSocketsApp morpheusApp
  let graphqlApi = httpPubApp [publish'] morpheusApp
  return (graphqlApi, wsApp, publish')