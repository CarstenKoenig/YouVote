module Polls.Stats exposing (..)

import Html as Html exposing (..)
import Html.Attributes as Attr
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
    , votes : Int
    }


initialModel : String -> Int -> ( Model, Cmd Msg )
initialModel urlBase pId =
    { pollId = pId
    , poll = Nothing
    , urlBase = urlBase
    }
        ! [ loadPoll urlBase pId ]


type
    Msg
    -- Nothing if there are no stats
    = LoadPollResult (Result Http.Error (Maybe Poll))


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
        LoadPollResult result ->
            case result of
                Err error ->
                    -- TODO: show error
                    model ! []

                Ok Nothing ->
                    model
                        ! [ Nav.modifyUrl
                                (Routing.routeToUrl
                                    (Routing.Vote model.pollId)
                                )
                          ]

                Ok (Just poll) ->
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
    Html.ul
        [ Attr.class "list-group" ]
        (List.map viewChoice poll.choices)


viewChoice : Choice -> Html Msg
viewChoice choice =
    Html.li
        [ Attr.class "list-group-item" ]
        [ Html.span [ Attr.class "badge" ] [ text (toString choice.votes) ]
        , text choice.choiceText
        ]


loadPoll : String -> Int -> Cmd Msg
loadPoll urlBase id =
    let
        mapChoice c =
            Choice c.choiceId c.answer (Maybe.withDefault 0 c.votes)

        statsIncluded c =
            case c.votes of
                Nothing ->
                    False

                Just _ ->
                    True

        mapPoll p =
            if Dict.values p.choices |> List.any statsIncluded then
                Just (Poll p.pollId p.question (Dict.values p.choices |> List.map mapChoice))
            else
                Nothing
    in
        Http.send
            (Result.map mapPoll >> LoadPollResult)
            (Api.getApiPollByPollId
                urlBase
                id
            )
