module Routing exposing (..)

import Navigation as Nav exposing (Location)
import UrlParser as Url exposing ((</>))


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
