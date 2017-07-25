module Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String
import Dict exposing (Dict)

type alias PollChoice =
    { choiceId : Int
    , answer : String
    , votes : Maybe (Int)
    }

decodePollChoice : Decoder PollChoice
decodePollChoice =
    decode PollChoice
        |> required "choiceId" int
        |> required "answer" string
        |> required "votes" (maybe int)

type alias Poll =
    { pollId : Int
    , question : String
    , choices : Dict (Int) (PollChoice)
    }

decodePoll : Decoder Poll
decodePoll =
    decode Poll
        |> required "pollId" int
        |> required "question" string
        |> required "choices" (map Dict.fromList (list (map2 (,) (index 0 int) (index 1 decodePollChoice))))

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

getApiPollList : String -> Http.Request (List (Poll))
getApiPollList urlBase =
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
                , "list"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodePoll)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getApiPollByPollId : String -> Int -> Http.Request (Poll)
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
            Http.expectJson decodePoll
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postApiPollByPollIdVoteByChoiceId : String -> Int -> Int -> Http.Request (Poll)
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
            Http.expectJson decodePoll
        , timeout =
            Nothing
        , withCredentials =
            False
        }

putApiPollCreate : String -> CreatePoll -> Http.Request (Poll)
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
            Http.expectJson decodePoll
        , timeout =
            Nothing
        , withCredentials =
            False
        }