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
    APIEvent,
  )
where

import Data.Morpheus.Subscriptions
  ( Event (..),
  )
import Import.NoFoundation

data Channel
  = Channel
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      Hashable
    )

type APIEvent = Event Channel MessagePersist
