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
import Heroicons.Solid exposing (heart, userCircle)
import Html exposing (Html, a, div, h1, pre, span, strong, text, textarea)
import Html.Attributes exposing (class, placeholder)
import RemoteData exposing (RemoteData)
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
    = GotResponse Model


type alias Model =
    RemoteData (Graphql.Http.Error Response) Response


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( RemoteData.Loading
    , makeRequest
    )


view : Model -> Html.Html Msg
view model =
    div []
        [ div [ class "flex flex-row h-screen" ]
            [ leftMenu
            , div [ class "flex-auto flex flex-row justify-around" ]
                [ div [ class "w-3/5 border-l border-r border-gray-400 flex flex-col" ]
                    [ div [ class "flex-none h-16 flex flex-row justify-between items-center p-5" ]
                        [ div [ class "" ] [ strong [] [ Html.text "User" ] ]
                        , div [ class "" ] [ heart [ Svg.Attributes.class "w-6 h-6 flex-none" ] ]
                        ]
                    , div [ class "flex-auto overflow-y-auto p-5" ] [ otherComment "Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi Hi " "vivshaw", otherComment "Hello" "vivshaw", otherComment "Howdy" "vivshaw" ]
                    , div [ class "flex-none h-40 p-5" ] [ textarea [ class "shadow-md w-full h-full outline-none border hover:border-blue-400 focus:border-blue-400 rounded p-3", placeholder "Hi" ] [] ]
                    ]
                ]
            ]
        , div []
            [ h1 [] [ text "Generated Query" ]
            , pre [] [ text (Document.serializeQuery query) ]
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
            [ div [ class "w-8 h-8 bg-gray-200 rounded-full" ] []
            , div [ class "w-8 h-8 bg-gray-200 rounded-full" ] []
            , div [ class "w-8 h-8 bg-gray-200 rounded-full" ] []
            , div [ class "w-8 h-8 bg-gray-200 rounded-full" ] []
            ]
        , div [ class "flex flex-col space-y-3" ]
            [ div [ class "w-8 h-8 bg-gray-200 rounded-full" ] []
            , div [ class "w-8 h-8 bg-gray-200 rounded-full" ] []
            , div [ class "w-8 h-8 bg-gray-200 rounded-full" ] []
            , div [ class "w-8 h-8 bg-gray-200 rounded-full" ] []
            ]
        ]


otherComment : String -> String -> Html.Html Msg
otherComment content user =
    div [ class "flex flex-row space-x-2" ]
        [ userCircle [ Svg.Attributes.class "w-6 h-6 flex-none" ]
        , div [ class "flex flex-col" ]
            [ div [ class "bg-gray-200 rounded p-5" ] [ Html.text content ]
            , div [ class "text-sm text-gray-500" ] [ Html.text user ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse response ->
            ( response, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
