module View exposing (root)

import Browser
import Heroicons.Solid exposing (chat, code, questionMarkCircle)
import Html exposing (Html, a, div)
import Html.Attributes exposing (class, classList, href)
import Pages.Chat exposing (pageChat)
import Pages.Help exposing (pageHelp)
import Pages.NotFound exposing (pageNotFound)
import Route exposing (Route)
import Svg exposing (svg)
import Svg.Attributes
import Types exposing (..)


{-| TEA View
This is the root view. All other views are called from here.
All this renders is the app shell.
Page layouts live in `src/Pages/`.
-}
root : Model -> Browser.Document Msg
root model =
    { title = "herc"
    , body =
        [ div []
            [ div [ class "flex flex-row h-screen" ]
                [ -- display the app menu...
                  leftMenu model.route
                , div [ class "flex-auto flex flex-row justify-around" ]
                    -- ...then route to pages by pattern matching on Route
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


{-| App menu.
Simply links to the Chat and Help routes, as well as the GitHub source.
-}
leftMenu : Route -> Html.Html Msg
leftMenu route =
    let
        matches =
            isRouteMatched route
    in
    div [ class "flex flex-col justify-between items-center w-16 p-3" ]
        [ div [ class "flex flex-col space-y-3" ]
            [ -- Match route to highlight the current route's link.
              -- FIXME: There must be a more elegant approach here.
              menuButton "/" "bg-green-400" chat (matches Route.Chat)
            , menuButton "/help" "bg-green-400" questionMarkCircle (matches Route.Help)
            ]
        , div [ class "flex flex-col space-y-3" ]
            [ menuButton "https://github.com/vivshaw/herc" "bg-purple-400" code True
            ]
        ]


{-| Simple boolean comparator on Routes
-}
isRouteMatched : Route -> Route -> Basics.Bool
isRouteMatched route route2 =
    route == route2


{-| Helper type for HeroIcons icons
-}
type alias HeroIcon =
    List (Svg.Attribute Msg) -> Html Msg


{-| A button from the app menu. Links to a URL, and displays a HeroIcon.
Background color depends on `isSelected`-
passing True means the color is always active,
passing False means it is always replaced with gray,
and passing a boolean comparator means you get conditional styling.
FIXME: Probably a tidier way to do this.
-}
menuButton : String -> String -> HeroIcon -> Basics.Bool -> Html.Html Msg
menuButton link color graphic isSelected =
    div
        [ classList
            [ ( "w-8 h-8 rounded-full flex flex-col items-center justify-center", True )
            , ( color, isSelected )
            , ( "bg-gray-400", not isSelected )
            ]
        ]
        [ a
            [ href link ]
            [ graphic [ Svg.Attributes.class "w-6 h-6 text-white" ] ]
        ]
