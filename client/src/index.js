import { Elm } from "./Main.elm";

import ApolloClient from "apollo-client";
import { split } from "apollo-link";
import { HttpLink } from "apollo-link-http";
import { WebSocketLink } from "apollo-link-ws";
import { getMainDefinition } from "apollo-utilities";
import { InMemoryCache } from "apollo-cache-inmemory";

import gql from "graphql-tag";

import "./styles/styles.css";

const getClient = () => {
  // Create an http link:
  const httpLink = new HttpLink({
    uri: `https://herc-server.herokuapp.com/graphql`,
  });

  // Create a WebSocket link:
  const wsLink = new WebSocketLink({
    uri: `wss://herc-server.herokuapp.com/`,
    options: {
      reconnect: true,
    },
  });

  // using the ability to split links, you can send data to each link
  // depending on what kind of operation is being sent
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

document.addEventListener("DOMContentLoaded", function () {
  var app = Elm.Main.init({
    node: document.getElementById("myapp"),
  });

  app.ports.createSubscriptionToMessages.subscribe(function (data) {
    /* Initiate subscription request */

    getClient()
      .subscribe({
        query: gql`
          ${data}
        `,
        variables: {},
      })
      .subscribe({
        next(resp) {
          app.ports.gotMessageSubscriptionData.send(resp);
        },
        error(err) {
          console.log("ws: error is: ", err);
        },
      });

    app.ports.socketStatusConnected.send(null);
  });
});
