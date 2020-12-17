{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( serve
    ) where

import Web.Scotty
import Network.HTTP.Types.Status (status404)

serve :: IO ()
serve = scotty 3000 $ do
    get "/api" $ do
        html "<h1>herc api is up</h1>"
    get "/graphql-playground" $ do
        setHeader "Content-Type" "text/html; charset=utf-8"
        file "src/views/graphql-playground.html"
    notFound $ do
        status status404
        text "Not found"
