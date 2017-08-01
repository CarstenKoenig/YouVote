module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Ev
import Http
import Navigation as Nav exposing (Location)
import UrlParser as Url exposing ((</>))
import Api exposing (..)
import Polls.New as NewPoll


type alias Model =
    { newPoll : Maybe NewPoll.Model
    , route : Route
    }


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
            { newPoll = Nothing
            , route = Root
            }
                ! []

        Just Create ->
            { newPoll = Just (NewPoll.initialModel "http://localhost:8080")
            , route = Create
            }
                ! []

        Nothing ->
            { newPoll = Nothing
            , route = Root
            }
                ! [ Nav.modifyUrl (routeToUrl Root) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPoll msg ->
            updateNewPoll msg model

        LocationChanged Nothing ->
            { model
                | newPoll = Nothing
                , route = Root
            }
                ! [ Nav.modifyUrl (routeToUrl Root) ]

        LocationChanged (Just Root) ->
            { model
                | newPoll = Nothing
                , route = Root
            }
                ! []

        LocationChanged (Just Create) ->
            { model
                | newPoll = Just (NewPoll.initialModel "http://localhost:8080")
                , route = Create
            }
                ! []

        NavigateTo route ->
            model ! [ Nav.newUrl (routeToUrl route) ]


updateNewPoll : NewPoll.Msg -> Model -> ( Model, Cmd Msg )
updateNewPoll msg model =
    case model.newPoll of
        Nothing ->
            model ! []

        Just newPoll ->
            let
                ( newPollUpdated, newPollCmd ) =
                    NewPoll.update msg newPoll
            in
                { model | newPoll = Just newPollUpdated } ! [ Cmd.map NewPoll newPollCmd ]


viewRoot : Model -> Html Msg
viewRoot _ =
    h1 [] [ text "Root" ]


view : Model -> Html Msg
view model =
    case model.newPoll of
        Nothing ->
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

        Just newPoll ->
            div []
                [ div [ Attr.class "row" ]
                    [ div [ Attr.class "col-sm-2" ] []
                    , div [ Attr.class "col-sm-10" ]
                        [ h3 [] [ text "create a new poll" ] ]
                    ]
                , NewPoll.view newPoll
                    |> Html.map NewPoll
                ]


type Route
    = Root
    | Create


parseLocation : Location -> Maybe Route
parseLocation location =
    let
        parser =
            Url.oneOf
                [ Url.map Root (Url.top)
                , Url.map Create (Url.s "create")
                ]
    in
        Url.parsePath parser location


routeToUrl : Route -> String
routeToUrl route =
    case route of
        Root ->
            "/"

        Create ->
            "/create"
