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
import Graphql.Types (APIEvent, Channel (..), ContentMsg (..))
import Import hiding (App, Query, publish, (.))

-- I am using schema-first GraphQL, so I must first import the schema.
-- FIXME: Would code-first be more elegant here?
importGQLDocument "schema.gql"

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
      publish [Event [Channel] ContentMsg {msgContent = content, msgAuthor = author, msgUuid = authorUuid}]
      lift (insertMessageDB SendMessageArgs {content, author, authorUuid})

    -- Subscription resolver.
    -- Is called only by `sendMessage`, and simply echoes back all Messages
    -- that are sent, to enable real-time chatting.
    messageSent :: SubscriptionField (ResolverS APIEvent Handler Message)
    messageSent = subscribe Channel $ do
      pure $ \(Event _ ContentMsg {msgContent, msgAuthor, msgUuid}) -> do
        pure
          Message
            { content = pure msgContent,
              author = pure msgAuthor,
              authorUuid = pure msgUuid
            }

-- Maps a single MessagePersist to a Morpheus GraphQL Message.
-- FIXME: Probably a more elegant way to do this.
toGqlMessage :: Monad m => MessagePersist -> Message m
toGqlMessage MessagePersist {messagePersistContent, messagePersistAuthor, messagePersistAuthorUuid} =
  Message
    { content = pure messagePersistContent,
      author = pure messagePersistAuthor,
      authorUuid = pure messagePersistAuthorUuid
    }

-- Handler to insert a single Message into the database.
-- Returns the inserted Message.
insertMessageDB :: SendMessageArgs -> Handler (Message (Resolver MUTATION APIEvent Handler))
insertMessageDB SendMessageArgs {content, author, authorUuid} = do
  insertedMsg <- runDB $ insertEntity MessagePersist {messagePersistContent = content, messagePersistAuthor = author, messagePersistAuthorUuid = authorUuid}
  pure $ toGqlMessage $ entityVal insertedMsg

-- Handler to return a list of all Messages in our database,
-- then fmap them to the appropriate format for our Query resolver.
-- Returns a List of Messages.
getAllMessagesDB :: Handler [Message (Resolver QUERY APIEvent Handler)]
getAllMessagesDB = do
  insertedMsg <- runDB $ selectList [] [Asc MessagePersistId]
  pure $ toGqlMessage . entityVal <$> insertedMsg

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