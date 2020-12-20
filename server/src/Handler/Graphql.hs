{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Graphql where

import Import
import Graphql.API
import Layout (graphqlLayout)
import Data.Morpheus.Types (GQLRequest)

getGraphqlR :: Handler Html
getGraphqlR = do
    graphqlLayout [whamlet|You made it to graphql!|]

postGraphqlR :: Handler Value
postGraphqlR = do
  body <- requireCheckJsonBody :: Handler GQLRequest
  result <- (liftIO . graphqlApi) body
  returnJson result
