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
    Resolver,
    ResolverM,
    ResolverS,
    RootResolver (..),
    SubscriptionField,
    publish,
    subscribe,
  )
import Data.Text (Text)
import Graphql.Types (APIEvent, Channel (..), ContentMsg (..))
import Import (Handler, HandlerFor, MessagePersist (..), handlerToIO, insertEntity, runDB)

importGQLDocument "schema.gql"

messageEvent :: Text -> Text -> Text -> APIEvent
messageEvent content author uuid = Event [Channel] (ContentMsg {msgContent = content, msgAuthor = author, msgUuid = uuid})

rootResolver :: RootResolver Handler APIEvent Query Mutation Subscription
rootResolver =
  RootResolver
    { queryResolver = Query {messages},
      mutationResolver = Mutation {sendMessage},
      subscriptionResolver = Subscription {messageSent}
    }
  where
    messages =
      pure
        []
    sendMessage :: SendMessageArgs -> ResolverM APIEvent Handler Message
    sendMessage SendMessageArgs {content, author, authorUuid} = do
      publish [messageEvent content author authorUuid]
      lift (setDBAddress SendMessageArgs {content, author, authorUuid})
    messageSent :: SubscriptionField (ResolverS APIEvent Handler Message)
    messageSent = subscribe Channel $ do
      pure $ \(Event _ content) -> do
        pure
          Message
            { content = pure (msgContent content),
              author = pure (msgAuthor content),
              authorUuid = pure (msgUuid content)
            }

setDBAddress :: SendMessageArgs -> Handler (Message (Resolver MUTATION APIEvent Handler))
setDBAddress SendMessageArgs {content, author, authorUuid} = do
  let msgPersist = MessagePersist {messagePersistContent = content, messagePersistAuthor = author, messagePersistAuthorUuid = authorUuid}

  pure
    Message
      { content = pure content,
        author = pure author,
        authorUuid = pure authorUuid
      }

morpheusApp :: App APIEvent Handler
morpheusApp = deriveApp rootResolver

getApi :: Handler (GQLRequest -> Handler GQLResponse, ServerApp, APIEvent -> Handler ())
getApi = do
  (wsApp, publish') <- webSocketsApp morpheusApp
  let graphqlApi = httpPubApp [publish'] morpheusApp
  return (graphqlApi, wsApp, publish')