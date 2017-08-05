module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Navigation as Nav exposing (Location)
import Routing exposing (..)
import Polls.New as NewPoll
import Polls.List as ListPolls
import Polls.Vote as VotePoll


type alias Model =
    { showing : Showing
    }


type Showing
    = ShowRoot ListPolls.Model
    | Creating NewPoll.Model
    | Voting VotePoll.Model


type Msg
    = NewPoll NewPoll.Msg
    | ListPolls ListPolls.Msg
    | VotePoll VotePoll.Msg
    | LocationChanged (Maybe Route)
    | NavigateTo Route


main : Program Never Model Msg
main =
    Nav.program
        (parseLocation >> LocationChanged)
        { init = init
        , subscriptions = always Sub.none
        , view = view
        , update = update
        }


init : Location -> ( Model, Cmd Msg )
init location =
    case
        parseLocation location
    of
        Just Root ->
            let
                ( listModel, listCmd ) =
                    (ListPolls.init "http://localhost:8080")
            in
                { showing = ShowRoot listModel
                }
                    ! [ Cmd.map ListPolls listCmd ]

        Just Create ->
            { showing = Creating (NewPoll.initialModel "http://localhost:8080")
            }
                ! []

        Just (Vote pId) ->
            let
                ( voteModel, voteCmd ) =
                    VotePoll.initialModel "http://localhost:8080" pId
            in
                { showing = Voting voteModel
                }
                    ! [ Cmd.map VotePoll voteCmd ]

        Nothing ->
            let
                ( listModel, listCmd ) =
                    (ListPolls.init "http://localhost:8080")
            in
                { showing = ShowRoot listModel
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

        LocationChanged Nothing ->
            let
                ( listModel, listCmd ) =
                    (ListPolls.init "http://localhost:8080")
            in
                { model
                    | showing = ShowRoot listModel
                }
                    ! [ Nav.modifyUrl (routeToUrl Root), Cmd.map ListPolls listCmd ]

        LocationChanged (Just Root) ->
            let
                ( listModel, listCmd ) =
                    (ListPolls.init "http://localhost:8080")
            in
                { model
                    | showing = ShowRoot listModel
                }
                    ! [ Cmd.map ListPolls listCmd ]

        LocationChanged (Just Create) ->
            { model
                | showing = Creating (NewPoll.initialModel "http://localhost:8080")
            }
                ! []

        LocationChanged (Just (Vote pId)) ->
            let
                ( voteModel, voteCmd ) =
                    VotePoll.initialModel "http://localhost:8080" pId
            in
                { showing = Voting voteModel
                }
                    ! [ Cmd.map VotePoll voteCmd ]

        NavigateTo route ->
            model ! [ Nav.newUrl (routeToUrl route) ]


updateNewPoll : NewPoll.Msg -> Model -> ( Model, Cmd Msg )
updateNewPoll msg model =
    case model.showing of
        Creating newPoll ->
            let
                ( newPollUpdated, newPollCmd ) =
                    NewPoll.update msg newPoll
            in
                { model | showing = Creating newPollUpdated } ! [ Cmd.map NewPoll newPollCmd ]

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
                { model | showing = Voting newVoteModel } ! [ Cmd.map VotePoll newVoteCmd ]

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
                ]
