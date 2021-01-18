{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Graphql where

import Data.Morpheus.Types (GQLRequest, GQLResponse)
import Graphql.API (getApi)
import Import
import Layout (graphqlLayout)

getGraphqlR :: Handler Html
getGraphqlR = do
  graphqlLayout ([whamlet|Empty Widget|])

postGraphqlR :: Handler Value
postGraphqlR = do
  App {graphqlApi} <- getYesod
  body <- requireCheckJsonBody :: Handler GQLRequest
  result <- graphqlApi body
  returnJson result
