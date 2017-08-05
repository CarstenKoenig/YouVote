module Polls.Vote exposing (..)

import Html as Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Ev
import Navigation as Nav
import Api exposing (..)
import Http
import Dict exposing (Dict)
import Routing


type alias Model =
    { pollId : Int
    , poll : Maybe Poll
    , urlBase : String
    }


type alias Poll =
    { pollId : Int
    , question : String
    , choices : List Choice
    }


type alias Choice =
    { choiceId : Int
    , choiceText : String
    }


initialModel : String -> Int -> ( Model, Cmd Msg )
initialModel urlBase pId =
    { pollId = pId
    , poll = Nothing
    , urlBase = urlBase
    }
        ! [ loadPoll urlBase pId ]


type Msg
    = NoOp
    | LoadPoll Int
      -- Nothing when already voted:
    | LoadPollResult (Result Http.Error (Maybe Poll))
    | VoteFor Int
    | VoteResult (Result Http.Error ())


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

        LoadPollResult result ->
            case result of
                Err error ->
                    -- TODO: show error
                    model ! []

                Ok Nothing ->
                    model
                        ! [ Nav.modifyUrl
                                (Routing.routeToUrl
                                    (Routing.Stats model.pollId)
                                )
                          ]

                Ok (Just poll) ->
                    { model
                        | pollId = poll.pollId
                        , poll = Just poll
                    }
                        ! []

        VoteFor choiceId ->
            model ! [ submitVote model.urlBase model.pollId choiceId ]

        VoteResult result ->
            case result of
                _ ->
                    model
                        ! [ Nav.modifyUrl
                                (Routing.routeToUrl
                                    (Routing.Stats model.pollId)
                                )
                          ]


view : Model -> Html Msg
view model =
    case model.poll of
        Nothing ->
            div [] []

        Just poll ->
            viewPoll poll


viewPoll : Poll -> Html Msg
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


viewChoices : Poll -> Html Msg
viewChoices poll =
    div
        [ Attr.class "list-group" ]
        (List.map viewChoice poll.choices)


viewChoice : Choice -> Html Msg
viewChoice choice =
    Html.button
        [ Ev.onClick (VoteFor choice.choiceId)
        , Attr.class "list-group-item"
        ]
        [ text choice.choiceText ]


loadPoll : String -> Int -> Cmd Msg
loadPoll urlBase id =
    let
        mapChoice c =
            Choice c.choiceId c.answer

        statsIncluded c =
            case c.votes of
                Nothing ->
                    False

                Just _ ->
                    True

        mapPoll p =
            if Dict.values p.choices |> List.any statsIncluded then
                Nothing
            else
                Just (Poll p.pollId p.question (Dict.values p.choices |> List.map mapChoice))
    in
        Http.send
            (Result.map mapPoll >> LoadPollResult)
            (Api.getApiPollByPollId
                urlBase
                id
            )


submitVote : String -> Int -> Int -> Cmd Msg
submitVote urlBase pollId choiceId =
    Http.send
        VoteResult
        (Api.postApiPollByPollIdVoteByChoiceId urlBase pollId choiceId)
