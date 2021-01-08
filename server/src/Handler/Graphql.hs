{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Graphql where

import Control.Monad.Trans.Reader (ReaderT (ReaderT))
import Data.Morpheus.Types (GQLRequest)
import Import
import Layout (graphqlLayout)
import Yesod.WebSockets

getGraphqlR :: Handler Html
getGraphqlR = do
  graphqlLayout ([whamlet|Empty Widget|])

postGraphqlR :: Handler Value
postGraphqlR = do
  App {graphqlApi} <- getYesod
  body <- requireCheckJsonBody :: Handler GQLRequest
  result <- (liftIO . graphqlApi) body
  returnJson result