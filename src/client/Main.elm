module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Ev
import Api exposing (..)
import Http
import Polls.New as NewPoll


type alias Model =
    { newPoll : NewPoll.Model
    }


type Msg
    = NewPoll NewPoll.Msg


main : Program Never Model Msg
main =
    program
        { init = Model (NewPoll.initialModel "http://localhost:8080") ! []
        , subscriptions = always Sub.none
        , view = view
        , update = update
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPoll msg ->
            let
                ( newPollUpdated, newPollCmd ) =
                    NewPoll.update msg model.newPoll
            in
                { model | newPoll = newPollUpdated } ! [ Cmd.map NewPoll newPollCmd ]


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Hallo Elm" ]
        , NewPoll.view model.newPoll
            |> Html.map NewPoll
        ]
