{-# LANGUAGE OverloadedStrings #-}

module Application (app) where

import API
import Web.Scotty
import Network.HTTP.Types.Status (status404)
import Control.Monad.IO.Class

app :: IO ()
app = scotty 3000 $ do
    post "/graphql" $ do
        setHeader "Content-Type" "application/json; charset=utf-8"
        raw =<< (liftIO . api =<< body)
    get "/graphql" $ do
        setHeader "Content-Type" "text/html; charset=utf-8"
        file "src/Views/graphql-playground.html"
    notFound $ do
        status status404
        text "Not found"
