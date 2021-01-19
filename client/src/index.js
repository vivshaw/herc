import { Elm } from "./Main.elm";

import ApolloClient from "apollo-client";
import { split } from "apollo-link";
import { HttpLink } from "apollo-link-http";
import { WebSocketLink } from "apollo-link-ws";
import { getMainDefinition } from "apollo-utilities";
import { InMemoryCache } from "apollo-cache-inmemory";

import gql from "graphql-tag";

import "./styles/styles.css";

// Fire when Elm attempts to connect to WebSockets.
// Returns an ApolloClient pointed at the herc server,
// and configured for WebSockets.
const getClient = () => {
  // Create an http link:
  const httpLink = new HttpLink({
    uri: `https://herc-server.herokuapp.com/`,
  });

  // Create a WebSocket link:
  const wsLink = new WebSocketLink({
    uri: `wss://herc-server.herokuapp.com/`,
    options: {
      reconnect: true,
    },
  });

  // FIXME: is splitting really necessary here?
  const link = split(
    // split based on operation type
    ({ query }) => {
      const definition = getMainDefinition(query);
      return (
        definition.kind === "OperationDefinition" &&
        definition.operation === "subscription"
      );
    },
    wsLink,
    httpLink
  );

  const client = new ApolloClient({
    link: link,
    cache: new InMemoryCache({
      addTypename: true,
    }),
  });
  return client;
};

// The action begins on page load!
document.addEventListener("DOMContentLoaded", function () {
  // Initialize Elm
  var app = Elm.Main.init({
    node: document.getElementById("myapp"),
  });

  // When Elm calls `createSubscriptionTomessages`...
  app.ports.createSubscriptionToMessages.subscribe(function (data) {
    getClient()
      // initiate a subscription request...
      .subscribe({
        query: gql`
          ${data}
        `,
        variables: {},
      })
      // then subscribe to the resulting Observable...
      .subscribe({
        next(resp) {
          // and pass Messages to Elm as they are observed.
          app.ports.gotMessageSubscriptionData.send(resp);
        },
        error(err) {
          console.log("ws: error is: ", err);
        },
      });

    // Tell Elm that WebSockets are ready!
    app.ports.socketStatusConnected.send(null);
  });
});
