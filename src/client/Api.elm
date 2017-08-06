module Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String
import Dict exposing (Dict)

type alias PollDescription =
    { pdId : Int
    , pdQuestion : String
    }

decodePollDescription : Decoder PollDescription
decodePollDescription =
    decode PollDescription
        |> required "pdId" int
        |> required "pdQuestion" string

type alias PollVote =
    { pvQuestion : String
    , pvChoices : Dict (Int) (String)
    }

decodePollVote : Decoder PollVote
decodePollVote =
    decode PollVote
        |> required "pvQuestion" string
        |> required "pvChoices" (dict string  |> map (Dict.toList >> List.filterMap (\( k, v ) -> String.toInt k |> Result.toMaybe |> Maybe.map (\i -> ( i, v ))) >> Dict.fromList))

type alias PollStat =
    { psQuestion : String
    , psVotes : List (Stat)
    }

decodePollStat : Decoder PollStat
decodePollStat =
    decode PollStat
        |> required "psQuestion" string
        |> required "psVotes" (list decodeStat)

type alias Stat =
    { vChoice : String
    , vVotes : Int
    }

decodeStat : Decoder Stat
decodeStat =
    decode Stat
        |> required "vChoice" string
        |> required "vVotes" int

type alias CreatePoll =
    { newQuestion : String
    , newChoices : List (String)
    }

encodeCreatePoll : CreatePoll -> Json.Encode.Value
encodeCreatePoll x =
    Json.Encode.object
        [ ( "newQuestion", Json.Encode.string x.newQuestion )
        , ( "newChoices", (Json.Encode.list << List.map Json.Encode.string) x.newChoices )
        ]

getApiPollsRecent : String -> Http.Request (List (PollDescription))
getApiPollsRecent urlBase =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "api"
                , "polls"
                , "recent"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodePollDescription)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getApiPollsMy : String -> Http.Request (List (PollDescription))
getApiPollsMy urlBase =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "api"
                , "polls"
                , "my"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodePollDescription)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getApiPollByPollId : String -> Int -> Http.Request (PollVote)
getApiPollByPollId urlBase capture_pollId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "api"
                , "poll"
                , capture_pollId |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodePollVote
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getApiPollByPollIdStats : String -> Int -> Http.Request (PollStat)
getApiPollByPollIdStats urlBase capture_pollId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "api"
                , "poll"
                , capture_pollId |> toString |> Http.encodeUri
                , "stats"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodePollStat
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postApiPollByPollIdVoteByChoiceId : String -> Int -> Int -> Http.Request (PollStat)
postApiPollByPollIdVoteByChoiceId urlBase capture_pollId capture_choiceId =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "api"
                , "poll"
                , capture_pollId |> toString |> Http.encodeUri
                , "vote"
                , capture_choiceId |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodePollStat
        , timeout =
            Nothing
        , withCredentials =
            False
        }

putApiPollCreate : String -> CreatePoll -> Http.Request (PollVote)
putApiPollCreate urlBase body =
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "api"
                , "poll"
                , "create"
                ]
        , body =
            Http.jsonBody (encodeCreatePoll body)
        , expect =
            Http.expectJson decodePollVote
        , timeout =
            Nothing
        , withCredentials =
            False
        }