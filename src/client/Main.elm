module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Navigation as Nav exposing (Location)
import Routing exposing (..)
import Polls.New as NewPoll
import Polls.List as ListPolls
import Polls.Vote as VotePoll
import Polls.Stats as StatPoll


type alias Model =
    { showing : Showing
    , baseUrl : String
    }


type alias Flags =
    { baseUrl : String }


type Showing
    = ShowRoot ListPolls.Model
    | Creating NewPoll.Model
    | Voting VotePoll.Model
    | ShowingStats StatPoll.Model


type Msg
    = NewPoll NewPoll.Msg
    | ListPolls ListPolls.Msg
    | VotePoll VotePoll.Msg
    | StatPoll StatPoll.Msg
    | LocationChanged (Maybe Route)


main : Program Flags Model Msg
main =
    Nav.programWithFlags
        (parseLocation >> LocationChanged)
        { init = init
        , subscriptions = always Sub.none
        , view = view
        , update = update
        }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        baseUrl =
            flags.baseUrl
    in
        case
            parseLocation location
        of
            Just Root ->
                let
                    ( listModel, listCmd ) =
                        (ListPolls.init baseUrl)
                in
                    { showing = ShowRoot listModel
                    , baseUrl = baseUrl
                    }
                        ! [ Cmd.map ListPolls listCmd ]

            Just Create ->
                { showing = Creating (NewPoll.initialModel baseUrl)
                , baseUrl = baseUrl
                }
                    ! []

            Just (Vote pId) ->
                let
                    ( voteModel, voteCmd ) =
                        VotePoll.initialModel baseUrl pId
                in
                    { showing = Voting voteModel
                    , baseUrl = baseUrl
                    }
                        ! [ Cmd.map VotePoll voteCmd ]

            Just (Stats pId) ->
                let
                    ( statsModel, statsCmd ) =
                        StatPoll.initialModel baseUrl pId
                in
                    { showing = ShowingStats statsModel
                    , baseUrl = baseUrl
                    }
                        ! [ Cmd.map StatPoll statsCmd ]

            Nothing ->
                let
                    ( listModel, listCmd ) =
                        (ListPolls.init baseUrl)
                in
                    { showing = ShowRoot listModel
                    , baseUrl = baseUrl
                    }
                        ! [ Nav.modifyUrl (routeToUrl Root), Cmd.map ListPolls listCmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPoll msg ->
            updateNewPoll msg model

        ListPolls msg ->
            updateListPolls msg model

        VotePoll msg ->
            updateVotePoll msg model

        StatPoll msg ->
            updateStatPoll msg model

        LocationChanged Nothing ->
            let
                ( listModel, listCmd ) =
                    (ListPolls.init model.baseUrl)
            in
                { model
                    | showing = ShowRoot listModel
                }
                    ! [ Nav.modifyUrl (routeToUrl Root), Cmd.map ListPolls listCmd ]

        LocationChanged (Just Root) ->
            let
                ( listModel, listCmd ) =
                    (ListPolls.init model.baseUrl)
            in
                { model
                    | showing = ShowRoot listModel
                }
                    ! [ Cmd.map ListPolls listCmd ]

        LocationChanged (Just Create) ->
            case model.showing of
                Creating _ ->
                    model ! []

                _ ->
                    { model | showing = Creating (NewPoll.initialModel model.baseUrl) } ! []

        LocationChanged (Just (Vote pId)) ->
            case model.showing of
                Voting _ ->
                    model ! []

                _ ->
                    let
                        ( voteModel, voteCmd ) =
                            VotePoll.initialModel model.baseUrl pId
                    in
                        { model | showing = Voting voteModel }
                            ! [ Cmd.map VotePoll voteCmd ]

        LocationChanged (Just (Stats pId)) ->
            case model.showing of
                ShowingStats _ ->
                    model ! []

                _ ->
                    let
                        ( statModel, statCmd ) =
                            StatPoll.initialModel model.baseUrl pId
                    in
                        { model | showing = ShowingStats statModel }
                            ! [ Cmd.map StatPoll statCmd ]


updateNewPoll : NewPoll.Msg -> Model -> ( Model, Cmd Msg )
updateNewPoll msg model =
    case model.showing of
        Creating newPoll ->
            let
                ( newPollUpdated, newPollCmd ) =
                    NewPoll.update msg newPoll
            in
                case msg of
                    NewPoll.PollCreated poll ->
                        { model | showing = Voting (VotePoll.modelFromPoll model.baseUrl poll) }
                            ! [ Nav.modifyUrl
                                    (Routing.routeToUrl (Routing.Vote poll.pollId))
                              ]

                    _ ->
                        { model | showing = Creating newPollUpdated }
                            ! [ Cmd.map NewPoll newPollCmd ]

        _ ->
            model ! []


updateListPolls : ListPolls.Msg -> Model -> ( Model, Cmd Msg )
updateListPolls msg model =
    case model.showing of
        ShowRoot listPolls ->
            let
                ( listUpdated, listCmd ) =
                    ListPolls.update msg listPolls
            in
                { model | showing = ShowRoot listUpdated }
                    ! [ Cmd.map ListPolls listCmd ]

        _ ->
            model ! []


updateVotePoll : VotePoll.Msg -> Model -> ( Model, Cmd Msg )
updateVotePoll msg model =
    case model.showing of
        Voting votePoll ->
            let
                ( newVoteModel, newVoteCmd ) =
                    VotePoll.update msg votePoll
            in
                case msg of
                    VotePoll.VoteCast poll ->
                        { model | showing = ShowingStats (StatPoll.modelFromPoll model.baseUrl poll) }
                            ! [ Nav.modifyUrl
                                    (Routing.routeToUrl (Routing.Stats poll.pollId))
                              ]

                    _ ->
                        { model | showing = Voting newVoteModel }
                            ! [ Cmd.map VotePoll newVoteCmd ]

        _ ->
            model ! []


updateStatPoll : StatPoll.Msg -> Model -> ( Model, Cmd Msg )
updateStatPoll msg model =
    case model.showing of
        ShowingStats statPoll ->
            let
                ( newStatModel, newStatCmd ) =
                    StatPoll.update msg statPoll
            in
                case msg of
                    StatPoll.PollNeedsVote poll ->
                        { model | showing = Voting (VotePoll.modelFromPoll model.baseUrl poll) }
                            ! [ Nav.modifyUrl
                                    (Routing.routeToUrl (Routing.Vote poll.pollId))
                              ]

                    _ ->
                        { model | showing = ShowingStats newStatModel }
                            ! [ Cmd.map StatPoll newStatCmd ]

        _ ->
            model ! []


viewRoot : Model -> Html Msg
viewRoot _ =
    h1 [] [ text "Root" ]


view : Model -> Html Msg
view model =
    case model.showing of
        ShowRoot listPolls ->
            div []
                [ div [ Attr.class "row" ]
                    [ div [ Attr.class "col-sm-2" ] []
                    , div [ Attr.class "col-sm-10" ]
                        [ h3 [] [ text "YouVote" ] ]
                    ]
                , div [ Attr.class "row" ]
                    [ div [ Attr.class "col-sm-2" ] []
                    , div [ Attr.class "col-sm-10" ]
                        [ ListPolls.view listPolls |> Html.map ListPolls ]
                    ]
                , div [ Attr.class "row" ]
                    [ div [ Attr.class "col-sm-2" ] []
                    , div [ Attr.class "col-sm-10" ]
                        [ a [ Attr.href (routeToUrl Create) ] [ text "new poll" ] ]
                    ]
                ]

        Creating newPoll ->
            div []
                [ div [ Attr.class "row" ]
                    [ div [ Attr.class "col-sm-2" ] []
                    , div [ Attr.class "col-sm-10" ]
                        [ h3 [] [ text "create a new poll" ] ]
                    ]
                , NewPoll.view newPoll
                    |> Html.map NewPoll
                , div [ Attr.class "row" ]
                    [ div [ Attr.class "col-sm-2" ] []
                    , div [ Attr.class "col-sm-10" ]
                        [ a [ Attr.href (routeToUrl Root) ] [ text "back" ] ]
                    ]
                ]

        Voting votePoll ->
            div []
                [ div [ Attr.class "row" ]
                    [ div [ Attr.class "col-sm-2" ] []
                    , div [ Attr.class "col-sm-10" ]
                        [ h3 [] [ text "cast your vote now" ] ]
                    ]
                , VotePoll.view votePoll
                    |> Html.map VotePoll
                , div [ Attr.class "row" ]
                    [ div [ Attr.class "col-sm-2" ] []
                    , div [ Attr.class "col-sm-10" ]
                        [ a [ Attr.href (routeToUrl Root) ] [ text "back" ] ]
                    ]
                ]

        ShowingStats statPoll ->
            div []
                [ div [ Attr.class "row" ]
                    [ div [ Attr.class "col-sm-2" ] []
                    , div [ Attr.class "col-sm-10" ]
                        [ h3 [] [ text "voting results" ] ]
                    ]
                , StatPoll.view statPoll
                    |> Html.map StatPoll
                , div [ Attr.class "row" ]
                    [ div [ Attr.class "col-sm-2" ] []
                    , div [ Attr.class "col-sm-10" ]
                        [ a [ Attr.href (routeToUrl Root) ] [ text "back" ] ]
                    ]
                ]
