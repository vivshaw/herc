{-# LANGUAGE OverloadedStrings #-}

module Application (app) where

import API
import Web.Scotty
import Network.HTTP.Types.Status (status404)
import Control.Monad.IO.Class

app :: IO ()
app = scotty 3000 $ do
    options "/graphql" $ do
        addHeader "Access-Control-Allow-Origin" ""
        addHeader "Access-Control-Allow-Methods" "POST"
        addHeader "Access-Control-Allow-Headers" "content-type"
        addHeader "Access-Control-Allow-Credentials" "true" 
    post "/graphql" $ do
        setHeader "Content-Type" "application/json; charset=utf-8"
        setHeader "Access-Control-Allow-Origin" "*"
        addHeader "Access-Control-Allow-Methods" "POST"
        addHeader "Access-Control-Allow-Credentials" "true"
        addHeader "Access-Control-Allow-Headers" "content-type"
        raw =<< (liftIO . api =<< body)
    get "/graphql" $ do
        setHeader "Content-Type" "text/html; charset=utf-8"
        file "src/Views/graphql-playground.html"
    notFound $ do
        status status404
        text "Not found"
