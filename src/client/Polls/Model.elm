module Polls.Model exposing (..)

import Api
import Dict


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
    { choiceText : String
    , votes : Int
    }


mapPollWithStat : Api.PollStat -> PollWithStats
mapPollWithStat pollData =
    let
        mapVote stat =
            ChoiceWithStats stat.vChoice stat.vVotes
    in
        { pollId = pollData.psId
        , question = pollData.psQuestion
        , choices = pollData.psVotes |> List.map mapVote
        }


mapPollWithoutStat : Api.PollVote -> PollWithoutStats
mapPollWithoutStat pollData =
    let
        mapChoice ( cId, cText ) =
            ChoiceWithoutStats cId cText
    in
        { pollId = pollData.pvId
        , question = pollData.pvQuestion
        , choices = Dict.toList pollData.pvChoices |> List.map mapChoice
        }
