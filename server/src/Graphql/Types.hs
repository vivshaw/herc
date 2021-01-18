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

module Graphql.Types
  ( Channel (..),
    ContentMsg (..),
    APIEvent,
  )
where

import Data.Morpheus.Subscriptions
  ( Event (..),
    Hashable,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

data Channel
  = Channel
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      Hashable
    )

data ContentMsg = ContentMsg {msgContent :: Text, msgAuthor :: Text, msgUuid :: Text}

type APIEvent = Event Channel ContentMsg
