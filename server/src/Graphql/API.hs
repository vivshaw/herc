{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Graphql.API (graphqlApi) where

import Data.Morpheus
  ( interpreter,
  )
import Data.Morpheus.Types (GQLRequest, GQLResponse, RootResolver (..), Undefined (..))
import Graphql.Schema.Query (Query, resolveQuery)

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = resolveQuery,
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

graphqlApi :: GQLRequest -> IO GQLResponse
graphqlApi = interpreter rootResolver