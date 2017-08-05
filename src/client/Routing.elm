module Routing exposing (..)

import Navigation as Nav exposing (Location)
import UrlParser as Url exposing ((</>))


type Route
    = Root
    | Create
    | Vote Int


parseLocation : Location -> Maybe Route
parseLocation location =
    let
        parser =
            Url.oneOf
                [ Url.map Root (Url.top)
                , Url.map Create (Url.s "create")
                , Url.map Vote (Url.s "poll" </> Url.int </> Url.s "vote")
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

        Vote id ->
            "/poll/" ++ toString id ++ "/vote"
