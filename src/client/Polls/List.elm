module Polls.List exposing (Model, Msg, init, update, view)

import Html as Html exposing (..)
import Html.Attributes as Attr
import Api exposing (..)
import Http


type alias Model =
    { polls : List Poll
    , urlBase : String
    , error : Maybe String
    }


type alias Poll =
    { question : String
    }


type Msg
    = NoOp
    | LoadListResult (Result Http.Error (List Api.Poll))


main : Program Never Model Msg
main =
    program
        { init = init "http://localhost:8080"
        , subscriptions = always Sub.none
        , view = view
        , update = update
        }


init : String -> ( Model, Cmd Msg )
init urlBase =
    { polls = []
    , urlBase = urlBase
    , error = Nothing
    }
        ! [ loadPolls urlBase ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        LoadListResult result ->
            case result of
                Err error ->
                    { model
                        | polls = []
                        , error = Just (toString error)
                    }
                        ! []

                Ok polls ->
                    { model
                        | error = Nothing
                        , polls = List.map mapPoll polls
                    }
                        ! []


view : Model -> Html Msg
view model =
    div []
        [ viewError model
        , ul [ Attr.class "list-group" ]
            (List.map viewPoll model.polls)
        ]


viewError : Model -> Html Msg
viewError model =
    case model.error of
        Nothing ->
            div [] []

        Just error ->
            div [ Attr.class "alert alert-error", Attr.attribute "role" "alert" ]
                [ text error ]


viewPoll : Poll -> Html Msg
viewPoll p =
    li [ Attr.class "list-group-item" ]
        [ text p.question ]


mapPoll : Api.Poll -> Poll
mapPoll p =
    Poll p.question


loadPolls : String -> Cmd Msg
loadPolls baseUri =
    Http.send LoadListResult (getApiPollList baseUri)
