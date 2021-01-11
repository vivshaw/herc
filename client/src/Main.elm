module Main exposing (..)

import Browser
import ChatAPI.Mutation as Mutation
import ChatAPI.Object exposing (Message)
import ChatAPI.Object.Message as Message exposing (author, content)
import ChatAPI.Query as Query
import ChatAPI.Scalar
import ChatAPI.ScalarCodecs
import Graphql.Document as Document
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Heroicons.Solid exposing (chat, heart, questionMarkCircle, userCircle)
import Html exposing (Html, a, button, div, input, text, textarea)
import Html.Attributes exposing (class, classList, href, placeholder, size, type_, value)
import Html.Events exposing (onClick, onInput)
import RemoteData exposing (RemoteData, WebData)
import Svg.Attributes



-- GraphQL Setup


type alias ChatMessage =
    { content : String
    , author : String
    }


type alias Response =
    List ChatMessage


endpoint : String
endpoint =
    "https://herc-graphql.herokuapp.com/graphql"


query : SelectionSet Response RootQuery
query =
    Query.messages messageSelection


messageSelection : SelectionSet ChatMessage ChatAPI.Object.Message
messageSelection =
    SelectionSet.map2 ChatMessage
        Message.content
        Message.author


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



-- Elm Architecture


type Msg
    = GotChat (RemoteData (Graphql.Http.Error Response) Response)
    | SendMessage ChatMessage
    | SentMessage (Result (Graphql.Http.Error ChatMessage) ChatMessage)
    | ChangeName String
    | ChangeComment String


type alias Model =
    { name : String
    , currentComment : String
    , chatMessages : RemoteData (Graphql.Http.Error Response) Response
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { name = "Anonymous User"
      , currentComment = ""
      , chatMessages = RemoteData.Loading
      }
    , makeRequest
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


viewMain : Model -> Html.Html Msg
viewMain model =
    div [ class "flex-auto flex flex-row justify-around" ]
        [ div [ class "w-3/5 border-l border-r border-gray-400 flex flex-col linedBg" ]
            [ div [ class "flex-none h-16 flex flex-row justify-between items-center p-5 bg-white border-b border-gray-400" ]
                [ div [ class "" ]
                    [ input
                        [ value model.name
                        , size (String.length model.name)
                        , type_ "text"
                        , class "flex-none outline-none font-semibold border-b-2 border-dashed"
                        , onInput ChangeName
                        ]
                        []
                    ]
                , div [ class "" ] [ heart [ Svg.Attributes.class "w-6 h-6 flex-none" ] ]
                ]
            , viewChat model
            , sendCommentBox model
            ]
        ]


viewChat : Model -> Html.Html Msg
viewChat model =
    case model.chatMessages of
        RemoteData.NotAsked ->
            div [ class "flex-auto overflow-y-auto p-5 space-y-2" ] [ text "Initialising." ]

        RemoteData.Loading ->
            div [ class "flex-auto overflow-y-auto p-5 space-y-2" ] [ text "Loading." ]

        RemoteData.Failure err ->
            div [ class "flex-auto overflow-y-auto p-5 space-y-2" ] [ text "Error!" ]

        RemoteData.Success chatMessages ->
            div [ class "flex-auto overflow-y-auto p-5 space-y-2" ] (List.map (comment model.name) chatMessages)


comment : String -> ChatMessage -> Html.Html Msg
comment name message =
    div
        [ classList
            [ ( "flex flex-row space-x-2", True )
            , ( "flex-row", message.author /= name )
            , ( "flex-row-reverse space-x-reverse", message.author == name )
            ]
        ]
        [ userCircle [ Svg.Attributes.class "w-6 h-6 flex-none" ]
        , div [ class "flex flex-col" ]
            [ div
                [ classList
                    [ ( "rounded p-5", True )
                    , ( "bg-gray-200", message.author /= name )
                    , ( "bg-green-200", message.author == name )
                    ]
                ]
                [ text message.content ]
            , div [ class "text-sm text-gray-500" ] [ text message.author ]
            ]
        ]


sendCommentBox : Model -> Html.Html Msg
sendCommentBox model =
    div [ class "flex-none h-50 p-5" ]
        [ div [ class "shadow-md bg-white w-full h-full border hover:border-green-400 focus:border-green-400 rounded p-3 flex flex-col items-start space-y-2" ]
            [ div [ class "font-semibold border-b-2 border-green-500 text-green-500 pb-1" ] [ text "Reply" ]
            , textarea [ class "w-full h-full outline-none resize-none", placeholder "Type your reply here.", value model.currentComment, onInput ChangeComment ] []
            , button [ class "self-end bg-green-400 hover:bg-green-600 text-white font-semibold py-1 px-1 rounded", onClick (SendMessage { author = model.name, content = model.currentComment }) ] [ text "Send" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                    case model.chatMessages of
                        RemoteData.Success messages ->
                            ( { model | chatMessages = RemoteData.Success (messages ++ [ chatMessage ]) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
