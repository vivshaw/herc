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

module Schema.Query (resolveQuery, Query) where

import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types (IORes)
import Data.Text (Text)

importGQLDocument "schema.gql"

resolveQuery :: Query (IORes ())
resolveQuery = Query { message, messages }
  where
    message MessageArgs {content} =
      pure
        Message
          { content = pure content,
            author = pure "Default Author"
          }
    messages =
      pure
        [
          Message
            {
              content = pure "Message 1",
              author = pure "Default Author"
            },
            Message
            {
              content = pure "Message 2",
              author = pure "Default Author"
            }
        ]