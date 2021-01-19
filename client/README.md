# herc client 💪

This is the Elm frontend for herc, hosted [here](https://herc-chat.netlify.app/). It is a real-time chat app, powered by GraphQL.

## Architecture

The client is an [Elm](https://elm-lang.org/) single page app. Much of the functionality is simply provided by Elm's core libraries. In addition, I'm using `krisajenkins/remotedata` and `dillonkearns/elm-graphql` to interact with my GraphQL backend.

The client uses ports to subscribe to a WebSocket via Javascript. For this, I'm using Apollo Client.

Lastly, I'm using Webpack to build it and to serve up a hot-reloading dev mode, and TailwindCSS with PostCSS for the styles.

## Setup

1. [Install Elm](https://guide.elm-lang.org/install/elm.html)
2. `npm install`

## Development

The frontend can be run in a hot-reloading dev mode with `npm run dev`. The app will be located at `http://localhost:3001` This also displays the Elm Debugger.

`elm-analyse` is available as well, with `npm run analyse`. It will be hosted at `http://localhost:3002`.

## Testing

```
npm run test
```

## Build

```
npm run build
```

## Deployment

The client is deployed to Netlify via a CI/CD pipeline. Currently, this is done directly with Netlify, and not via the CircleCI instance.

## To-Do

- Consider moving deployment inside CircleCI
- Revise styling
