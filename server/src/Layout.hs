{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Layout where

import Import
import Text.Hamlet (hamletFile)

graphqlLayout :: Widget -> Handler Html
graphqlLayout _ = do
  pc <- widgetToPageContent $ do
    $(widgetFile "graphql/graphql-layout")
  withUrlRenderer $(hamletFile "templates/graphql/graphql-layout-wrapper.hamlet")