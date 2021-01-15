module Pages.Chat exposing (pageChat)

import Heroicons.Solid exposing (userCircle)
import Html exposing (Html, a, button, div, input, p, text, textarea)
import Html.Attributes exposing (class, classList, placeholder, size, type_, value)
import Html.Events exposing (onClick, onInput)
import RemoteData exposing (RemoteData)
import Svg.Attributes
import Types exposing (..)


pageChat : Model -> Html.Html Msg
pageChat model =
    div [ class "w-full lg:w-3/5 border-l border-r border-gray-400 flex flex-col linedBg" ]
        [ div [ class "flex-none h-16 flex flex-row justify-between items-center p-5 bg-white border-b border-gray-400" ]
            [ div [ class "flex flex-col items-start" ]
                [ input
                    [ value model.name
                    , size (String.length model.name)
                    , type_ "text"
                    , class "flex-none outline-none font-semibold border-b-2 border-dashed"
                    , onInput ChangeName
                    ]
                    []
                , case model.uuid of
                    Just uuid ->
                        div [ class "pt-1" ] [ chip True (abbreviateUuid uuid) ]

                    Nothing ->
                        div [ class "pt-1" ] [ text "Initializing..." ]
                ]
            ]
        , viewChat model
        , sendCommentBox model
        ]


abbreviateUuid : String -> String
abbreviateUuid uuid =
    String.slice 0 4 uuid ++ "..." ++ String.slice -4 (String.length uuid) uuid


chip : Bool -> String -> Html.Html Msg
chip self uuid =
    case self of
        True ->
            div [ class "flex justify-center items-center m-1 font-medium py-1 px-2 bg-white rounded-full text-green-700 bg-green-100 border border-green-300" ]
                [ div [ class "text-xs font-normal leading-none flex-initial" ]
                    [ text uuid ]
                ]

        False ->
            div [ class "flex justify-center items-center m-1 font-medium py-1 px-2 bg-white rounded-full text-gray-700 bg-gray-100 border border-gray-300" ]
                [ div [ class "text-xs font-normal leading-none flex-initial" ]
                    [ text uuid ]
                ]


viewChat : Model -> Html.Html Msg
viewChat model =
    case model.uuid of
        Just uuid ->
            case model.chatMessages of
                RemoteData.NotAsked ->
                    div [ class "flex-auto overflow-y-auto p-5 space-y-2" ] [ text "Initialising." ]

                RemoteData.Loading ->
                    div [ class "flex-auto overflow-y-auto p-5 space-y-2" ] [ text "Loading." ]

                RemoteData.Failure err ->
                    div [ class "flex-auto overflow-y-auto p-5 space-y-2" ] [ text "Error!" ]

                RemoteData.Success chatMessages ->
                    div [ class "flex-auto overflow-y-auto p-5 space-y-2" ] (List.map (comment uuid) chatMessages)

        Nothing ->
            div [ class "flex-auto overflow-y-auto p-5 space-y-2" ] [ text "Initialising." ]


comment : String -> ChatMessage -> Html.Html Msg
comment uuid message =
    let
        isMe =
            message.authorUuid == uuid
    in
    div
        [ classList
            [ ( "flex flex-row space-x-2", True )
            , ( "flex-row-reverse space-x-reverse", isMe )
            , ( "flex-row", not isMe )
            ]
        ]
        [ userCircle [ Svg.Attributes.class "w-6 h-6 flex-none" ]
        , div [ class "flex flex-col" ]
            [ div
                [ classList
                    [ ( "rounded p-5", True )
                    , ( "bg-green-200", isMe )
                    , ( "bg-gray-200", not isMe )
                    ]
                ]
                [ text message.content ]
            , div [ class "flex items-center" ]
                [ div [ class "text-sm text-gray-500" ] [ text message.author ]
                , chip isMe (abbreviateUuid message.authorUuid)
                ]
            ]
        ]


sendCommentBox : Model -> Html.Html Msg
sendCommentBox model =
    case model.uuid of
        Just uuid ->
            div [ class "flex-none h-50 p-5" ]
                [ div [ class "shadow-md bg-white w-full h-full border hover:border-green-400 focus:border-green-400 rounded p-3 flex flex-col items-start space-y-2" ]
                    [ div [ class "font-semibold border-b-2 border-green-500 text-green-500 pb-1" ] [ text "Reply" ]
                    , textarea [ class "w-full h-full outline-none resize-none", placeholder "Type your reply here.", value model.currentComment, onInput ChangeComment ] []
                    , button [ class "self-end bg-green-400 hover:bg-green-600 text-white font-semibold py-1 px-1 rounded", onClick (SendMessage { author = model.name, content = model.currentComment, authorUuid = uuid }) ] [ text "Send" ]
                    ]
                ]

        Nothing ->
            div [ class "flex-none h-50 p-5" ]
                [ div [ class "shadow-md bg-white w-full h-full border hover:border-green-400 focus:border-green-400 rounded p-3 flex flex-col items-start space-y-2" ]
                    [ div [ class "font-semibold border-b-2 border-green-500 text-green-500 pb-1" ] [ text "Reply" ]
                    , textarea [ class "w-full h-full outline-none resize-none", placeholder "Type your reply here.", value model.currentComment, onInput ChangeComment ] []
                    , button [ class "self-end bg-green-400 hover:bg-green-600 text-white font-semibold py-1 px-1 rounded" ] [ text "Initializing..." ]
                    ]
                ]
