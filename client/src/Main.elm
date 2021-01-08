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
import Html exposing (Html, a, div, h1, pre, span, text)
import Html.Attributes exposing (class)
import RemoteData exposing (RemoteData)



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
            [ div [ class "flex flex-col justify-between items-center w-16 p-3" ]
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
            , div [ class "flex-auto flex flex-row justify-around bg-blue-200" ]
                [ div [ class "w-3/5 border-l border-r border-gray-400 flex flex-col" ]
                    [ div [ class "flex-none h-16 bg-red-200" ] []
                    , div [ class "flex-auto overflow-y-auto bg-blue-200" ] []
                    , div [ class "flex-none h-64 bg-green-200" ] []
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
