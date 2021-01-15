port module Main exposing (..)

import Browser
import ChatAPI.Mutation as Mutation
import ChatAPI.Object exposing (Message)
import ChatAPI.Object.Message as Message exposing (author, content)
import ChatAPI.Query as Query
import ChatAPI.Scalar
import ChatAPI.ScalarCodecs
import ChatAPI.Subscription as Subscription
import Graphql.Document as Document
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Heroicons.Solid exposing (chat, questionMarkCircle, sparkles, userCircle)
import Html exposing (Html, a, button, div, input, text, textarea)
import Html.Attributes exposing (class, classList, href, placeholder, size, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Random
import RemoteData exposing (RemoteData, WebData)
import Svg.Attributes
import UUID exposing (UUID)



-- GraphQL Setup


type alias ChatMessage =
    { content : String
    , author : String
    , authorUuid : String
    }


type alias Response =
    List ChatMessage


endpoint : String
endpoint =
    "https://herc-server.herokuapp.com/graphql"


query : SelectionSet Response RootQuery
query =
    Query.messages messageSelection


messageSelection : SelectionSet ChatMessage ChatAPI.Object.Message
messageSelection =
    SelectionSet.map3 ChatMessage
        Message.content
        Message.author
        Message.authorUuid


makeRequest : Cmd Msg
makeRequest =
    query
        |> Graphql.Http.queryRequest endpoint
        |> Graphql.Http.send (RemoteData.fromResult >> GotChat)


sendChatMessage : ChatMessage -> SelectionSet ChatMessage RootMutation
sendChatMessage message =
    Mutation.sendMessage message messageSelection


doMutation : SelectionSet ChatMessage RootMutation -> Cmd Msg
doMutation mutation =
    mutation
        |> Graphql.Http.mutationRequest endpoint
        |> Graphql.Http.send SentMessage


subscriptionDocument : SelectionSet ChatMessage RootSubscription
subscriptionDocument =
    Subscription.messageSent messageSelection



-- Randomness Setup


seedRng : Random.Generator Random.Seed
seedRng =
    Random.int Random.minInt Random.maxInt
        |> Random.map Random.initialSeed



-- Elm Architecture


type Msg
    = GotChat (RemoteData (Graphql.Http.Error Response) Response)
    | SendMessage ChatMessage
    | SentMessage (Result (Graphql.Http.Error ChatMessage) ChatMessage)
    | ChangeName String
    | ChangeComment String
    | NewSubscriptionStatus SubscriptionStatus ()
    | MessageSubscriptionDataReceived Json.Decode.Value
    | SeedResult UUID


type SubscriptionStatus
    = NotConnected
    | Connected


type alias Model =
    { name : String
    , currentComment : String
    , chatMessages : RemoteData (Graphql.Http.Error Response) Response
    , subscriptionStatus : SubscriptionStatus
    , uuid : Maybe String
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { name = "Anonymous User"
      , currentComment = ""
      , chatMessages = RemoteData.Loading
      , subscriptionStatus = NotConnected
      , uuid = Nothing
      }
    , Cmd.batch
        [ makeRequest
        , createSubscriptionToMessages (subscriptionDocument |> Document.serializeSubscription)
        , Random.generate SeedResult UUID.generator
        ]
    )


view : Model -> Html.Html Msg
view model =
    div []
        [ div [ class "flex flex-row h-screen" ]
            [ leftMenu
            , viewMain model
            ]
        ]


leftMenu : Html.Html Msg
leftMenu =
    div [ class "flex flex-col justify-between items-center w-16 p-3" ]
        [ div [ class "flex flex-col space-y-3" ]
            [ div [ class "w-8 h-8 bg-green-400 rounded-full flex flex-col items-center justify-center" ]
                [ chat [ Svg.Attributes.class "w-6 h-6 text-white" ]
                ]
            ]
        , div [ class "flex flex-col space-y-3" ]
            [ div [ class "w-8 h-8 bg-gray-400 rounded-full flex flex-col items-center justify-center" ]
                [ a [ href "https://github.com/vivshaw/herc" ] [ questionMarkCircle [ Svg.Attributes.class "w-6 h-6 text-white" ] ]
                ]
            ]
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


viewMain : Model -> Html.Html Msg
viewMain model =
    div [ class "flex-auto flex flex-row justify-around" ]
        [ div [ class "w-3/5 border-l border-r border-gray-400 flex flex-col linedBg" ]
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
                , div [ class "" ] [ sparkles [ Svg.Attributes.class "w-6 h-6 flex-none" ] ]
                ]
            , viewChat model
            , sendCommentBox model
            ]
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SeedResult uuid ->
            ( { model | uuid = Just (UUID.toString uuid) }, Cmd.none )

        GotChat response ->
            ( { model | chatMessages = response }, Cmd.none )

        ChangeName newName ->
            ( { model | name = newName }, Cmd.none )

        ChangeComment newComment ->
            ( { model | currentComment = newComment }, Cmd.none )

        SendMessage message ->
            ( { model | currentComment = "" }
            , doMutation (sendChatMessage message)
            )

        SentMessage message ->
            case message of
                Ok chatMessage ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NewSubscriptionStatus newStatus () ->
            ( { model | subscriptionStatus = newStatus }, Cmd.none )

        MessageSubscriptionDataReceived newData ->
            case Json.Decode.decodeValue (subscriptionDocument |> Document.decoder) newData of
                Ok chatMessage ->
                    case model.chatMessages of
                        RemoteData.Success messages ->
                            ( { model | chatMessages = RemoteData.Success (messages ++ [ chatMessage ]) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Err error ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ gotMessageSubscriptionData MessageSubscriptionDataReceived
        , socketStatusConnected (NewSubscriptionStatus Connected)
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


port createSubscriptionToMessages : String -> Cmd msg


port socketStatusConnected : (() -> msg) -> Sub msg


port gotMessageSubscriptionData : (Json.Decode.Value -> msg) -> Sub msg
