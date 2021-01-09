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
import Control.Monad.Reader
import Data.Morpheus
  ( App,
    deriveApp,
    runApp,
  )
import Data.Morpheus.Document (RootResolverConstraint, importGQLDocument)
import Data.Morpheus.Subscriptions
  ( Event (..),
    Hashable,
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
    lift,
    publish,
    subscribe,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data Channel
  = ChannelA
  | ChannelB
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      Hashable
    )

data Content
  = ContentA Int
  | ContentB Text
  | ContentC Text

type APIEvent = Event Channel Content

messageEvent :: APIEvent
messageEvent = Event [ChannelA] (ContentA 1)

importGQLDocument "schema.gql"

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
      publish [messageEvent]
      liftIO $ putStrLn "Send Message"
      lift (setDBAddress SendMessageArgs {content, author})
    messageSent :: SubscriptionField (ResolverS APIEvent IO Message)
    messageSent = subscribe ChannelA $ do
      liftIO $ putStrLn "Init message subscription"
      pure $ \(Event _ content) -> do
        liftIO $ putStrLn "MessageSent"
        pure
          Message
            { content = pure "Message 1",
              author = pure "Default Author"
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
