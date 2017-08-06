module Polls.Stats exposing (Model, Msg, initialModel, modelFromPoll, update, view)

import Html as Html exposing (..)
import Html.Attributes as Attr
import Navigation as Nav
import Api
import Http
import Routing
import Polls.Model exposing (..)


type alias Model =
    { pollId : Int
    , poll : Maybe PollWithStats
    , urlBase : String
    }


initialModel : String -> Int -> ( Model, Cmd Msg )
initialModel urlBase pId =
    { pollId = pId
    , poll = Nothing
    , urlBase = urlBase
    }
        ! [ loadPoll urlBase pId ]


modelFromPoll : String -> PollWithStats -> Model
modelFromPoll baseUrl poll =
    { pollId = poll.pollId
    , poll = Just poll
    , urlBase = baseUrl
    }


type Msg
    = PollLoaded PollWithStats
    | PollLoadingError String


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
        PollLoadingError error ->
            -- there was an error - try to goto vote instead
            model ! [ Nav.modifyUrl (Routing.routeToUrl (Routing.Vote model.pollId)) ]

        PollLoaded poll ->
            { model
                | pollId = poll.pollId
                , poll = Just poll
            }
                ! []


view : Model -> Html Msg
view model =
    case model.poll of
        Nothing ->
            div [] []

        Just poll ->
            viewPoll poll


viewPoll : PollWithStats -> Html Msg
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


viewChoices : PollWithStats -> Html Msg
viewChoices poll =
    Html.ul
        [ Attr.class "list-group" ]
        (poll.choices |> List.sortBy (negate << .votes) |> List.map viewChoice)


viewChoice : ChoiceWithStats -> Html Msg
viewChoice choice =
    Html.li
        [ Attr.class "list-group-item" ]
        [ Html.span [ Attr.class "badge" ] [ text (toString choice.votes) ]
        , text choice.choiceText
        ]


loadPoll : String -> Int -> Cmd Msg
loadPoll urlBase pollId =
    let
        selectMsg res =
            case res of
                Err error ->
                    PollLoadingError (toString error)

                Ok poll ->
                    PollLoaded poll
    in
        Http.send
            (Result.map mapPollWithStat >> selectMsg)
            (Api.getApiPollByPollIdStats
                urlBase
                pollId
            )
