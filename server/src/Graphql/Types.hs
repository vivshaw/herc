{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Graphql.Types
  ( Channel (..),
    APIEvent,
  )
where

import Data.Morpheus.Subscriptions
  ( Event (..),
  )
import Import.NoFoundation

-- The PubSub channel over which the GraphQL subscription events are sent.
data Channel
  = Channel
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      Hashable
    )

-- The Events which will be sent over the PubSub channel.
-- As the Msg to be sent, I'm using the Persistent model directly for tidiness.
type APIEvent = Event Channel MessagePersist
