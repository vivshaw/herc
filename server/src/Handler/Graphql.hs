{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Graphql where

import Data.Morpheus.Types (GQLRequest)
import Graphql.API
import Import
import Layout (graphqlLayout)

getGraphqlR :: Handler Html
getGraphqlR = do
  graphqlLayout ([whamlet|Empty Widget|])

postGraphqlR :: Handler Value
postGraphqlR = do
  body <- requireCheckJsonBody :: Handler GQLRequest
  result <- (liftIO . graphqlApi) body
  returnJson result
