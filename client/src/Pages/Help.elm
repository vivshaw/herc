module Pages.Help exposing (pageHelp)

import Html exposing (div, p, text)
import Html.Attributes exposing (class)
import Types exposing (..)


pageHelp : Html.Html Msg
pageHelp =
    div [ class "w-full lg:w-3/5 border-l border-r border-gray-400 flex flex-col linedBg" ]
        [ div [ class "flex-none h-16 flex flex-row justify-between items-center p-5 bg-white border-b border-gray-400" ]
            [ div [ class "flex flex-col items-start" ]
                [ div
                    [ class "flex-none outline-none font-semibold"
                    ]
                    [ text "Haskell & Elm Rudimentary Chat" ]
                ]
            ]
        , div [ class "p-5" ]
            [ p [ class "pb-3" ] [ text "Welcome to Haskell & Elm Rudimentary Chat (herc), a pure-functional realtime chat app! herc runs on an Elm <- Graphql <- Haskell stack, with the backend built on Yesod." ]
            , p [ class "pb-3" ] [ text "In herc, you're identified by a UUID (generated on pageload). You may also select a user nickname. Click on \"Anonymous User\" in the title bar to edit your nick. Messages you wrote will be highlighted in green, while those written by others are highlighted in grey." ]
            , p [ class "pb-3" ] [ text "Send messages via the \"Reply\" box." ]
            ]
        ]
