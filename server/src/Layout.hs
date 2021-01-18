{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Layout where

import Import
import Text.Hamlet (hamletFile)

-- Render the GraphQL Playground for interactive querying & testing.
graphqlLayout :: Handler Html
graphqlLayout = do
  -- Pull app root from Yesod settings. This will be passed into the widget
  -- to register the GraphQL URLs.
  App {appSettings} <- getYesod
  let root = getRoot $ appRoot appSettings

  pc <- widgetToPageContent $ do
    $(widgetFile "graphql-layout")
  withUrlRenderer $(hamletFile "templates/graphql-layout-wrapper.hamlet")
  where
    -- Get the app root if provided, and use localhost if not.
    getRoot :: Maybe Text -> Text
    getRoot (Just root) = root
    getRoot Nothing = "http://localhost:3000"