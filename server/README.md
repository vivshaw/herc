# herc server 💪

This is the GraphQL backend service that powers [the herc chat app](https://herc-chat.netlify.app/). It provides real-time chat via Subscriptions over WebSockets, alongside the usual Queries and Mutations.

If you'd like to explore the GraphQL API, there is [a GraphQL Playground](https://herc-server.herokuapp.com/) available.

## Architecture

herc server was built with [Yesod](https://www.yesodweb.com/) due to its convenient, batteries-included nature, and [Morpheus GraphQL](https://morpheusgraphql.com/) for the GraphQL toolkit. The database is currently just SQLite, with [Persistent](https://hackage.haskell.org/package/persistent) as my data layer.

The GraphQL portion of the app lives in `src/Graphql`. The rest is all pretty normal Yesod!

GraphQL `Queries` and `Mutations` are handled within Yesod, inside the `GraphqlR` route handler. `Subscriptions` are instead exposed as a separate WAI app that is stood up alongside Yesod with `websocketsOr`.

## Haskell Setup

1. [Install Stack](https://haskell-lang.org/get-started)
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

## Development

Start a live-refreshing dev server with:

```
stack exec -- yesod devel
```

The app will be located at `YESOD_APPROOT` if defined, otherwise `http://localhost:3000`. A [GraphQL Playground](https://github.com/graphql/graphql-playground) is accessible at the server root URL.

## Testing

```
stack test --flag my-project:library-only --flag my-project:dev
```

## Build

```
stack build
```

## Deployment

The server is deployed to Heroku through a CircleCI CI/CD pipeline. Currently, CircleCI Dockerizes the app, builds a **static Linux x86_64 binary**, tests it, and then **deploys only the static binary**. This [requires some odd little tricks](https://www.fpcomplete.com/blog/2016/10/static-compilation-with-stack/), but it's worth it, as it removes the need to rebuild on Heroku.

## To-Do

- Consider fully Dockerized deployment in place of static binaries
- Conditional workflow in CircleCI
- Add REST routes if needed
- Swap to PostgresQL
