{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.GraphqlSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
  describe "Homepage" $ do
    it "loads the index and checks it looks right" $ do
      get GraphqlR
      statusIs 200
      htmlAnyContain "title" "herc GraphQL Playground"
