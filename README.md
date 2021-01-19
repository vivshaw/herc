# herc (Haskell & Elm Rudimentary Chat) 💪

[herc](https://herc-chat.netlify.app/) is a learning project for getting up to speed with Elm! It is a real-time chat app with Elm frontend and Haskell backend, using GraphQL to enjoy type-safety throughout the stack. 👍

You can also [explore the API](https://herc-server.herokuapp.com/) via GraphQL Playground.

## herc client

[herc client](client) is an Elm single page app. I'm using Webpack to build it and serve up a hot-reloading dev mode, and Apollo Client for GraphQL subscriptions over WebSockets. Please see [the client readme](client/README.md) for more details and installation instructions.

## herc server

[herc server](server) is written in Haskell. I'm using `yesod` as the web framework, and `Morpheus` for GraphQL support. Please see [the server readme](server/README.md) for more details and installation instructions.

## Deployment

Both parts of the app are deployed via CI/CD pipline. The server goes to CircleCI and from there to Heroku, and the client goes direct to Netlify.
