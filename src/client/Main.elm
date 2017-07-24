module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Ev
import Api exposing (..)
import Http


type alias Model =
    { a : String
    , b : String
    , result : Int
    }


type Msg
    = InputA String
    | InputB String
    | Execute
    | GotResult (Result Http.Error Int)


main : Program Never Model Msg
main =
    program
        { init = { a = "", b = "", result = 0 } ! []
        , subscriptions = always Sub.none
        , view = view
        , update = update
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputA a ->
            { model | a = a } ! []

        InputB b ->
            { model | b = b } ! []

        Execute ->
            let
                intA =
                    String.toInt model.a

                intB =
                    String.toInt model.b
            in
                case Result.map2 Addition intA intB of
                    Err _ ->
                        model ! []

                    Ok add ->
                        let
                            request =
                                postApiAdd "http://localhost:8080" add
                        in
                            model ! [ Http.send GotResult request ]

        GotResult result ->
            case result of
                Err _ ->
                    model ! []

                Ok res ->
                    { model | result = res } ! []


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Hallo Elm" ]
        , input [ Attr.type_ "text", Ev.onInput InputA, Attr.value model.a ] []
        , input [ Attr.type_ "text", Ev.onInput InputB, Attr.value model.b ] []
        , button [ Ev.onClick Execute ] [ text "+" ]
        , h4 [] [ text (toString model.result) ]
        ]
