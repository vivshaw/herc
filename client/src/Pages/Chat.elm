module Pages.Chat exposing (pageChat)

import Heroicons.Solid exposing (userCircle)
import Html exposing (button, div, form, input, text, textarea)
import Html.Attributes exposing (class, classList, placeholder, size, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import RemoteData
import Svg.Attributes
import Types exposing (..)


{-| The Chat page.
The main screen of the app.
Contains a header, a chat message window, and a control to send messages.
-}
pageChat : Model -> Html.Html Msg
pageChat model =
    div [ class "w-full lg:w-3/5 border-l border-r border-gray-400 flex flex-col linedBg" ]
        [ -- Header
          header model

        -- Chat message window
        , viewChat model

        -- Send message control
        , sendMessageBox model
        ]


{-| The app's header bar.
This allows the user to change their name,
as well as dislays their UUID.
-}
header : Model -> Html.Html Msg
header model =
    div [ class "flex-none h-16 flex flex-row justify-between items-center p-5 bg-white border-b border-gray-400" ]
        [ div [ class "flex flex-col items-start" ]
            [ -- Input to control name. Emits ChangeName
              input
                [ value model.name
                , size (String.length model.name)
                , type_ "text"
                , class "flex-none outline-none font-semibold border-b-2 border-dashed"
                , onInput ChangeName
                ]
                []
            , case model.uuid of
                Just uuid ->
                    -- If user has a UUID, display it.
                    div [ class "pt-1" ] [ chip True (abbreviateUuid uuid) ]

                Nothing ->
                    -- If user does not yet have a UUID, wait.
                    div [ class "pt-1" ] [ text "Initializing..." ]
            ]
        ]


{-| Helper function to abbreviate a UUID
into just its first 4 and last 4 characters.
Used to shrink them to fit into chips.
-}
abbreviateUuid : String -> String
abbreviateUuid uuid =
    String.slice 0 4 uuid ++ "..." ++ String.slice -4 (String.length uuid) uuid


{-| A chip for displaying UUIDs.
Color is conditional on whether the UUID matches the active user.
FIXME: Display full UUID on hover
-}
chip : Bool -> String -> Html.Html Msg
chip isMe uuid =
    div
        [ classList
            [ ( "flex justify-center items-center m-1 font-medium py-1 px-2 rounded-full border", True )
            , ( "text-green-700 bg-green-100 border-green-300", isMe )
            , ( "text-gray-700 bg-gray-100 border-gray-300", not isMe )
            ]
        ]
        [ div [ class "text-xs font-normal leading-none flex-initial" ]
            [ text uuid ]
        ]


{-| Chat window.
Displays all the messages the user receives.
-}
viewChat : Model -> Html.Html Msg
viewChat model =
    case model.uuid of
        Just uuid ->
            -- We need UUID to identify who sent what!
            case model.chatMessages of
                RemoteData.NotAsked ->
                    -- If GraphQL isn't ready yet, wait
                    div [ class "flex-auto overflow-y-auto p-5 space-y-2" ] [ text "Initialising." ]

                RemoteData.Loading ->
                    -- If messages query is loading, wait
                    div [ class "flex-auto overflow-y-auto p-5 space-y-2" ] [ text "Loading." ]

                RemoteData.Failure _ ->
                    -- Failed to query.
                    -- FIXME: Better error handling here.
                    div [ class "flex-auto overflow-y-auto p-5 space-y-2" ] [ text "Error!" ]

                RemoteData.Success chatMessages ->
                    -- Success! Display the chat window, with the messages inside.
                    div [ class "flex-auto overflow-y-auto p-5 space-y-2" ] (List.map (message uuid) chatMessages)

        Nothing ->
            -- If user doesn't have a UUID yet, wait.
            div [ class "flex-auto overflow-y-auto p-5 space-y-2" ] [ text "Initialising." ]


{-| Chat message component.
Is aligned and colored conditionally,
depending on whether the active user sent this message.
-}
message : String -> ChatMessage -> Html.Html Msg
message uuid chatMessage =
    let
        -- Simple boolean comparator on UUID
        isMe =
            chatMessage.authorUuid == uuid
    in
    div
        [ classList
            -- Flip alignment depending on isMe
            [ ( "flex flex-row space-x-2", True )
            , ( "flex-row-reverse space-x-reverse", isMe )
            , ( "flex-row", not isMe )
            ]
        ]
        [ userCircle [ Svg.Attributes.class "w-6 h-6 flex-none" ]
        , div [ class "flex flex-col" ]
            [ div
                [ classList
                    -- Swap color based on isMe
                    [ ( "rounded p-5", True )
                    , ( "bg-green-200", isMe )
                    , ( "bg-gray-200", not isMe )
                    ]
                ]
                [ text chatMessage.content ]
            , div [ class "flex items-center" ]
                [ -- display message author
                  div [ class "text-sm text-gray-500" ] [ text chatMessage.author ]

                -- display author UUID in a chip
                , chip isMe (abbreviateUuid chatMessage.authorUuid)
                ]
            ]
        ]


{-| Send message control.
-}
sendMessageBox : Model -> Html.Html Msg
sendMessageBox model =
    let
        -- A user needs a UUID to send messages!
        -- As such, the Send button and the form itself
        -- are conditional on model.uuid.
        sendButton =
            case model.uuid of
                Just uuid ->
                    -- If the the user has a UUID, ready to go!
                    button
                        [ class "self-end bg-green-400 hover:bg-green-600 text-white font-semibold py-1 px-1 rounded" ]
                        [ text "Send" ]

                Nothing ->
                    -- If the user does not have a UUID yet, wait.
                    button
                        [ class "self-end bg-green-400 hover:bg-green-600 text-white font-semibold py-1 px-1 rounded" ]
                        [ text "Initializing..." ]

        -- Message form. Emits SendMessage.
        messageForm =
            case model.uuid of
                Just uuid ->
                    -- If the user has a UUID, enable sending messages on submit.
                    form
                        [ class "shadow-md bg-white w-full h-full border hover:border-green-400 focus:border-green-400 rounded p-3 flex flex-col items-start space-y-2"
                        , onSubmit (SendMessage { author = model.name, content = model.currentContent, authorUuid = uuid })
                        ]

                Nothing ->
                    -- If the user does not have a UUID yet, disable submitting.
                    form
                        [ class "shadow-md bg-white w-full h-full border hover:border-green-400 focus:border-green-400 rounded p-3 flex flex-col items-start space-y-2"
                        , onSubmit NoOp
                        ]
    in
    div [ class "flex-none h-50 p-5" ]
        [ messageForm
            [ div
                [ class "font-semibold border-b-2 border-green-500 text-green-500 pb-1" ]
                [ text "Reply" ]

            -- Input component for message content. Emits ChangeContent.
            , input
                [ class "w-full h-full outline-none resize-none"
                , placeholder "Type your reply here."
                , value model.currentContent
                , onInput ChangeContent
                ]
                []

            -- Send button. Triggers onSubmit.
            , sendButton
            ]
        ]
