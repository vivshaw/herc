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
    Hashable,
  )
import Data.Morpheus.Types
  ( MUTATION,
    Resolver,
    ResolverM,
    ResolverS,
    RootResolver (..),
    SubscriptionField,
    publish,
    subscribe,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

importGQLDocument "schema.gql"

data Channel
  = ChannelMessage
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      Hashable
    )

data Content = ContentMessage {msgContent :: Text, msgAuthor :: Text, msgUuid :: Text}

type APIEvent = Event Channel Content

messageEvent :: Text -> Text -> Text -> APIEvent
messageEvent content author uuid = Event [ChannelMessage] (ContentMessage {msgContent = content, msgAuthor = author, msgUuid = uuid})

rootResolver :: RootResolver IO APIEvent Query Mutation Subscription
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
    sendMessage :: SendMessageArgs -> ResolverM APIEvent IO Message
    sendMessage SendMessageArgs {content, author, authorUuid} = do
      publish [messageEvent content author authorUuid]
      lift (setDBAddress SendMessageArgs {content, author, authorUuid})
    messageSent :: SubscriptionField (ResolverS APIEvent IO Message)
    messageSent = subscribe ChannelMessage $ do
      pure $ \(Event _ content) -> do
        pure
          Message
            { content = pure (msgContent content),
              author = pure (msgAuthor content),
              authorUuid = pure (msgUuid content)
            }

setDBAddress :: SendMessageArgs -> IO (Message (Resolver MUTATION APIEvent IO))
setDBAddress SendMessageArgs {content, author, authorUuid} = do
  pure
    Message
      { content = pure content,
        author = pure author,
        authorUuid = pure authorUuid
      }

morpheusApp :: App APIEvent IO
morpheusApp = deriveApp rootResolver
