module Route exposing (Route(..), parseUrl)

import Url exposing (Url)
import Url.Parser exposing (..)


{-| herc currently has 3 routes:

1.  The Chat page, at '/'
2.  the Help page, at '/help'
3.  the 404 page, for all other routes

-}
type Route
    = NotFound
    | Chat
    | Help


{-| Attempt to parse a URL, and default to NotFound if it's not a known route.
-}
parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


{-| Attempts to match a Route against the provided routes.
Will return Nothing is the route is unknown.
-}
matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Chat top
        , map Help (s "help")
        ]
