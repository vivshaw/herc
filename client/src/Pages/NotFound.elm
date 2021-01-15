module Pages.NotFound exposing (pageNotFound)

import Html exposing (Html, a, div, p, text)
import Html.Attributes exposing (class, href)
import Types exposing (..)


pageNotFound : Html.Html Msg
pageNotFound =
    div [ class "w-full lg:w-3/5 border-l border-r border-gray-400 flex flex-col linedBg" ]
        [ div [ class "container flex flex-col md:flex-row items-center justify-center px-5 text-gray-700" ]
            [ div [ class "pt-6 max-w-md" ]
                [ div [ class "text-5xl font-dark font-bold" ] [ text "404" ]
                , p [ class "pt-2 text-2xl md:text-3xl font-light leading-normal" ] [ text "Sorry, there's no such page." ]
                , p [ class "pt-1 mb-8" ] [ text "No worries- you can just return to the chat!" ]
                , a [ href "/", class "px-4 inline py-2 text-sm font-medium leading-5 shadow text-white transition-colors duration-150 border border-transparent rounded-lg focus:outline-none focus:shadow-outline-green bg-green-400 active:bg-green-400 hover:bg-green-600" ] [ text "back to chat" ]
                ]
            ]
        ]
