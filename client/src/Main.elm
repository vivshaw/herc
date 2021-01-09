module Main exposing (..)

import Browser
import ChatAPI.Object exposing (Message)
import ChatAPI.Object.Message as Message exposing (author, content)
import ChatAPI.Query as Query
import ChatAPI.Scalar
import ChatAPI.ScalarCodecs
import Graphql.Document as Document
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Heroicons.Solid exposing (chat, heart, questionMarkCircle, userCircle)
import Html exposing (Html, a, button, div, h1, pre, span, strong, text, textarea)
import Html.Attributes exposing (class, classList, href, placeholder)
import RemoteData exposing (RemoteData, WebData)
import Svg.Attributes



-- GraphQL Setup


type alias Response =
    List Message


type alias Message =
    { content : String
    , author : String
    }


query : SelectionSet (List Message) RootQuery
query =
    Query.messages messageSelection


messageSelection : SelectionSet Message ChatAPI.Object.Message
messageSelection =
    SelectionSet.map2 Message
        Message.content
        Message.author


makeRequest : Cmd Msg
makeRequest =
    query
        |> Graphql.Http.queryRequest
            "http://127.0.0.1:3000/graphql"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)



-- Elm Architecture Setup


type Msg
    = GotResponse (RemoteData (Graphql.Http.Error Response) Response)
    | ChangeName String


type alias Model =
    { name : String
    , chatMessages : RemoteData (Graphql.Http.Error Response) Response
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { name = "Anonymous User"
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
        , div []
            [ h1 [] [ text "Response" ]
            , Html.text (Debug.toString model)
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
                [ div [ class "" ] [ strong [] [ text model.name ] ]
                , div [ class "" ] [ heart [ Svg.Attributes.class "w-6 h-6 flex-none" ] ]
                ]
            , viewChat model
            , sendCommentBox
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


comment : String -> Message -> Html.Html Msg
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


sendCommentBox : Html.Html Msg
sendCommentBox =
    div [ class "flex-none h-50 p-5" ]
        [ div [ class "shadow-md bg-white w-full h-full border hover:border-green-400 focus:border-green-400 rounded p-3 flex flex-col items-start space-y-2" ]
            [ div [ class "font-semibold border-b-2 border-green-500 text-green-500 pb-1" ] [ text "Reply" ]
            , textarea [ class "w-full h-full outline-none resize-none", placeholder "Type your reply here." ] []
            , button [ class "self-end bg-green-400 hover:bg-green-600 text-white font-semibold py-1 px-1 rounded" ] [ text "Send" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse response ->
            ( { model | chatMessages = response }, Cmd.none )

        ChangeName newName ->
            ( { model | name = newName }, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
