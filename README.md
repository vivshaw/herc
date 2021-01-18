# herc (Haskell & Elm Rudimentary Chat)

This is a learning project for getting up to speed with Elm frontends and Haskell backends! When fully functional, it will be a realtime chat app. I'm using GraphQL to enjoy type-safety throughout the stack. 👍

## Client

The client is an Elm SPA. I'm using Webpack to build it and serve up a hot-reloading dev mode, and Apollo Client for GraphQL subscriptions over WebSockets.

## Server

The server is written in Haskell. I'm using `yesod` as the web framework, and `Morpheus` for GraphQL support.

## Installing

The frontend can be run in dev mode with `cd client && npm run dev`, or built with the usual `npm run build`.

The server can be run in dev mode with `cd server && stack exec -- yesod devel`, or built with the usual `stack build`.
