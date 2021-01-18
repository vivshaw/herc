{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Graphql where

import Data.Morpheus.Types (GQLRequest)
import Import
import Layout (graphqlLayout)

-- GET route displays GraphQL Playground
getGraphqlR :: Handler Html
getGraphqlR = graphqlLayout

-- POST route handles GraphQL queries
postGraphqlR :: Handler Value
postGraphqlR = do
  App {graphqlApi} <- getYesod
  body <- requireCheckJsonBody :: Handler GQLRequest
  result <- graphqlApi body
  returnJson result
