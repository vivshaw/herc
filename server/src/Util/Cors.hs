{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Util.Cors (allowCors) where

import Import
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors

-- WAI Middleware to allow CORS requests.
allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

-- I extend the simpleCorsResourcePolicy with the HTTP methods and headers
-- needed for GraphQL and WebSockets.
appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
  simpleCorsResourcePolicy
    { corsMethods = ["OPTIONS", "GET", "PUT", "POST"],
      corsRequestHeaders = ["Authorization", "Content-Type"]
    }