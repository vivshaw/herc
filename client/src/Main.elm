module Main exposing (..)

import Browser
import State
import Types exposing (..)
import View


main : Program Flags Model Msg
main =
    Browser.application
        { init = State.init
        , update = State.update
        , subscriptions = State.subscriptions
        , view = View.root
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
