{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Layout where

import Import
import Text.Hamlet          (hamletFile)

graphqlLayout :: Widget -> Handler Html
graphqlLayout widget = do
    pc <- widgetToPageContent $ do
        $(widgetFile "graphql/graphql-layout")
    withUrlRenderer $(hamletFile "templates/graphql/graphql-layout-wrapper.hamlet")