module View exposing (root)

import Browser
import Heroicons.Solid exposing (chat, code, questionMarkCircle)
import Html exposing (a, div)
import Html.Attributes exposing (class, href)
import Pages.Chat exposing (pageChat)
import Pages.Help exposing (pageHelp)
import Pages.NotFound exposing (pageNotFound)
import Route exposing (Route)
import Svg.Attributes
import Types exposing (..)


root : Model -> Browser.Document Msg
root model =
    { title = "herc"
    , body =
        [ div []
            [ div [ class "flex flex-row h-screen" ]
                [ leftMenu model.route
                , div [ class "flex-auto flex flex-row justify-around" ]
                    [ case model.route of
                        Route.Chat ->
                            pageChat model

                        Route.Help ->
                            pageHelp

                        Route.NotFound ->
                            pageNotFound
                    ]
                ]
            ]
        ]
    }


leftMenu : Route -> Html.Html Msg
leftMenu route =
    div [ class "flex flex-col justify-between items-center w-16 p-3" ]
        [ div [ class "flex flex-col space-y-3" ]
            [ case route of
                Route.Chat ->
                    div [ class "w-8 h-8 bg-green-400 rounded-full flex flex-col items-center justify-center" ]
                        [ a [ href "/" ] [ chat [ Svg.Attributes.class "w-6 h-6 text-white" ] ]
                        ]

                _ ->
                    div [ class "w-8 h-8 bg-gray-400 rounded-full flex flex-col items-center justify-center" ]
                        [ a [ href "/" ] [ chat [ Svg.Attributes.class "w-6 h-6 text-white" ] ]
                        ]
            , case route of
                Route.Help ->
                    div [ class "w-8 h-8 bg-green-400 rounded-full flex flex-col items-center justify-center" ]
                        [ a [ href "/help" ] [ questionMarkCircle [ Svg.Attributes.class "w-6 h-6 text-white" ] ]
                        ]

                _ ->
                    div [ class "w-8 h-8 bg-gray-400 rounded-full flex flex-col items-center justify-center" ]
                        [ a [ href "/help" ] [ questionMarkCircle [ Svg.Attributes.class "w-6 h-6 text-white" ] ]
                        ]
            ]
        , div [ class "flex flex-col space-y-3" ]
            [ div [ class "w-8 h-8 bg-gray-400 rounded-full flex flex-col items-center justify-center bg-purple-400" ]
                [ a [ href "https://github.com/vivshaw/herc" ] [ code [ Svg.Attributes.class "w-6 h-6 text-white" ] ]
                ]
            ]
        ]
