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

module Graphql.Schema.Query (resolveQuery, Query) where

import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types (IORes)
import Data.Text (Text)

importGQLDocument "schema.gql"

resolveQuery :: Query (IORes ())
resolveQuery = Query {messages}
  where
    messages =
      pure
        [ Message
            { content = pure "Message 1",
              author = pure "Default Author"
            },
          Message
            { content = pure "Message 2",
              author = pure "Default Author"
            },
          Message
            { content = pure "Message 3",
              author = pure "Anonymous User"
            }
        ]