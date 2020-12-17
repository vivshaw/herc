{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( serve
    ) where

import Web.Scotty

import Data.Monoid (mconcat)

serve :: IO ()
serve = scotty 3000 $
    get "/api" $ do
        html "<h1>herc api is up</h1>"
