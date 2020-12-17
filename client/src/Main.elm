module Main exposing (..)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)


main : Html a
main =
    span [ class "welcome-message" ] [ text "Hello, World!" ]
    