module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Navigation as Nav exposing (Location)
import Routing exposing (..)
import Polls.New as NewPoll


type alias Model =
    { showing : Showing
    }


type Showing
    = ShowRoot
    | Creating NewPoll.Model


type Msg
    = NewPoll NewPoll.Msg
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
            { showing = ShowRoot
            }
                ! []

        Just Create ->
            { showing = Creating (NewPoll.initialModel "http://localhost:8080")
            }
                ! []

        Nothing ->
            { showing = ShowRoot
            }
                ! [ Nav.modifyUrl (routeToUrl Root) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPoll msg ->
            updateNewPoll msg model

        LocationChanged Nothing ->
            { model
                | showing = ShowRoot
            }
                ! [ Nav.modifyUrl (routeToUrl Root) ]

        LocationChanged (Just Root) ->
            { model
                | showing = ShowRoot
            }
                ! []

        LocationChanged (Just Create) ->
            { model
                | showing = Creating (NewPoll.initialModel "http://localhost:8080")
            }
                ! []

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


viewRoot : Model -> Html Msg
viewRoot _ =
    h1 [] [ text "Root" ]


view : Model -> Html Msg
view model =
    case model.showing of
        ShowRoot ->
            div []
                [ div [ Attr.class "row" ]
                    [ div [ Attr.class "col-sm-2" ] []
                    , div [ Attr.class "col-sm-10" ]
                        [ h3 [] [ text "YouVote" ] ]
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
