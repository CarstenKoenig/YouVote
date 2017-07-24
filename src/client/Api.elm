module Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias Addition =
    { operandA : Int
    , operandB : Int
    }

decodeAddition : Decoder Addition
decodeAddition =
    decode Addition
        |> required "operandA" int
        |> required "operandB" int

encodeAddition : Addition -> Json.Encode.Value
encodeAddition x =
    Json.Encode.object
        [ ( "operandA", Json.Encode.int x.operandA )
        , ( "operandB", Json.Encode.int x.operandB )
        ]

postApiAdd : String -> Addition -> Http.Request (Int)
postApiAdd urlBase body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ urlBase
                , "api"
                , "add"
                ]
        , body =
            Http.jsonBody (encodeAddition body)
        , expect =
            Http.expectJson int
        , timeout =
            Nothing
        , withCredentials =
            False
        }