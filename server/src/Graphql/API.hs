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

data Content = ContentMessage {msgContent :: Text, msgAuthor :: Text}

type APIEvent = Event Channel Content

messageEvent :: Text -> Text -> APIEvent
messageEvent content author = Event [ChannelMessage] (ContentMessage {msgContent = content, msgAuthor = author})

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
        [ Message
            { content = pure "Message 1",
              author = pure "Default Author"
            },
          Message
            { content = pure "Message 2",
              author = pure "Default Author"
            }
        ]
    sendMessage :: SendMessageArgs -> ResolverM APIEvent IO Message
    sendMessage SendMessageArgs {content, author} = do
      publish [messageEvent content author]
      lift (setDBAddress SendMessageArgs {content, author})
    messageSent :: SubscriptionField (ResolverS APIEvent IO Message)
    messageSent = subscribe ChannelMessage $ do
      pure $ \(Event _ content) -> do
        pure
          Message
            { content = pure (msgContent content),
              author = pure (msgAuthor content)
            }

setDBAddress :: SendMessageArgs -> IO (Message (Resolver MUTATION APIEvent IO))
setDBAddress SendMessageArgs {content, author} = do
  pure
    Message
      { content = pure content,
        author = pure author
      }

morpheusApp :: App APIEvent IO
morpheusApp = deriveApp rootResolver
