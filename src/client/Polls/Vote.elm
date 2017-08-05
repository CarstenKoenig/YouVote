module Polls.Vote exposing (..)

import Html as Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Ev
import Api
import Http
import Polls.Model exposing (..)


type alias Model =
    { pollId : Int
    , poll : Maybe PollWithoutStats
    , urlBase : String
    }


initialModel : String -> Int -> ( Model, Cmd Msg )
initialModel urlBase pId =
    { pollId = pId
    , poll = Nothing
    , urlBase = urlBase
    }
        ! [ loadPoll urlBase pId ]


modelFromPoll : String -> PollWithoutStats -> Model
modelFromPoll baseUrl poll =
    { pollId = poll.pollId
    , poll = Just poll
    , urlBase = baseUrl
    }


type Msg
    = NoOp
    | LoadPoll Int
    | PollLoaded Poll
    | PollLoadingError Http.Error
    | VoteFor Int
    | VoteCast Poll
    | VoteError Http.Error


main : Program Never Model Msg
main =
    let
        ( model, cmd ) =
            initialModel "http://localhost:8080" 1
    in
        program
            { init = model ! [ cmd ]
            , subscriptions = always Sub.none
            , view = view
            , update = update
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        LoadPoll id ->
            { model
                | pollId = id
                , poll = Nothing
            }
                ! [ loadPoll model.urlBase id ]

        PollLoadingError error ->
            -- TODO: show error
            model ! []

        PollLoaded (WithStats _) ->
            -- should be handeled in main-handler
            model ! []

        PollLoaded (WithoutStats poll) ->
            { model
                | pollId = poll.pollId
                , poll = Just poll
            }
                ! []

        VoteFor choiceId ->
            model ! [ submitVote model.urlBase model.pollId choiceId ]

        VoteCast (WithStats poll) ->
            -- should be handeled in main-handler
            model ! []

        VoteCast (WithoutStats poll) ->
            -- TODO: this is an error
            { model
                | pollId = poll.pollId
                , poll = Just poll
            }
                ! []

        VoteError error ->
            -- TODO: show Error
            model ! []


view : Model -> Html Msg
view model =
    case model.poll of
        Nothing ->
            div [] []

        Just poll ->
            viewPoll poll


viewPoll : PollWithoutStats -> Html Msg
viewPoll poll =
    div []
        [ div [ Attr.class "row" ]
            [ div [ Attr.class "col-sm-2" ] []
            , div [ Attr.class "col-sm-10" ] [ h2 [] [ text poll.question ] ]
            ]
        , div [ Attr.class "row" ]
            [ div [ Attr.class "col-sm-2" ] []
            , div [ Attr.class "col-sm-10" ] [ viewChoices poll ]
            ]
        ]


viewChoices : PollWithoutStats -> Html Msg
viewChoices poll =
    div
        [ Attr.class "list-group" ]
        (List.map viewChoice poll.choices)


viewChoice : ChoiceWithoutStats -> Html Msg
viewChoice choice =
    Html.button
        [ Ev.onClick (VoteFor choice.choiceId)
        , Attr.class "list-group-item"
        ]
        [ text choice.choiceText ]


loadPoll : String -> Int -> Cmd Msg
loadPoll urlBase id =
    let
        selectMsg res =
            case res of
                Err err ->
                    PollLoadingError err

                Ok poll ->
                    PollLoaded poll
    in
        Http.send
            (Result.map mapPoll >> selectMsg)
            (Api.getApiPollByPollId
                urlBase
                id
            )


submitVote : String -> Int -> Int -> Cmd Msg
submitVote urlBase pollId choiceId =
    let
        selectMsg res =
            case res of
                Ok poll ->
                    VoteCast poll

                Err err ->
                    VoteError err
    in
        Http.send
            (Result.map mapPoll >> selectMsg)
            (Api.postApiPollByPollIdVoteByChoiceId urlBase pollId choiceId)
