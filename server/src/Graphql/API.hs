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

module Graphql.API (graphqlApi) where

import Data.Morpheus
  ( interpreter,
  )
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Subscriptions
  ( Event (..),
  )
import Data.Morpheus.Types (GQLRequest, GQLResponse, IORes, MUTATION, Resolver, ResolverM, ResolverS, RootResolver (..), SubscriptionField, Undefined (..), lift, publish, subscribe)
import Data.Text (Text)

data Channel = MessageChannel

newtype Content = Content {contentID :: Int}

type APIEvent = Event Channel Content

messageEvent :: APIEvent
messageEvent = Event [MessageChannel] (Content {contentID = 1})

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
      lift setDBAddress
    messageSent :: SubscriptionField (ResolverS APIEvent IO Message)
    messageSent = subscribe MessageChannel $ do
      pure $ \(Event _ content) -> do
        pure
          Message
            { content = pure "Message 1",
              author = pure "Default Author"
            }

setDBAddress :: IO (Message (Resolver MUTATION APIEvent IO))
setDBAddress = do
  pure
    Message
      { content = pure "New Message",
        author = pure "New Author"
      }

graphqlApi :: GQLRequest -> IO GQLResponse
graphqlApi = interpreter rootResolver