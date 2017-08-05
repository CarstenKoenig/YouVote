module Polls.Model exposing (..)

import Api
import Dict


type Poll
    = WithoutStats PollWithoutStats
    | WithStats PollWithStats


type alias PollWithoutStats =
    { pollId : Int
    , question : String
    , choices : List ChoiceWithoutStats
    }


type alias PollWithStats =
    { pollId : Int
    , question : String
    , choices : List ChoiceWithStats
    }


type alias ChoiceWithoutStats =
    { choiceId : Int
    , choiceText : String
    }


type alias ChoiceWithStats =
    { choiceId : Int
    , choiceText : String
    , votes : Int
    }


mapPoll : Api.Poll -> Poll
mapPoll p =
    let
        mapChoice c =
            ChoiceWithStats c.choiceId c.answer (Maybe.withDefault 0 c.votes)

        mapChoiceWithout c =
            ChoiceWithoutStats c.choiceId c.answer

        statsIncluded c =
            case c.votes of
                Nothing ->
                    False

                Just _ ->
                    True
    in
        if Dict.values p.choices |> List.any statsIncluded then
            WithStats
                { pollId = p.pollId
                , question = p.question
                , choices = Dict.values p.choices |> List.map mapChoice
                }
        else
            WithoutStats
                { pollId = p.pollId
                , question = p.question
                , choices = Dict.values p.choices |> List.map mapChoiceWithout
                }
