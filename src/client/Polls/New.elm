module Polls.New exposing (..)

import Html as Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Ev
import Api exposing (..)
import Http
import Dict exposing (Dict)


type alias Model =
    { question : String
    , choices : Dict Int String
    , urlBase : String
    }


initialModel : String -> Model
initialModel urlBase =
    { question = ""
    , choices = Dict.singleton 1 ""
    , urlBase = urlBase
    }


type Msg
    = NoOp
    | EditQuestion String
    | EditChoice Int String
    | Execute
    | ExecuteResult (Result Http.Error Api.Poll)


main : Program Never Model Msg
main =
    program
        { init = initialModel "http://localhost:8080" ! []
        , subscriptions = always Sub.none
        , view = view
        , update = update
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        EditQuestion text ->
            { model | question = text } ! []

        EditChoice index text ->
            changeText index text model ! []

        Execute ->
            if validModel model then
                let
                    choices =
                        validChoices model

                    data =
                        Api.CreatePoll model.question choices
                in
                    model
                        ! [ Http.send
                                ExecuteResult
                                (Api.putApiPollCreate
                                    model.urlBase
                                    data
                                )
                          ]
            else
                model ! []

        ExecuteResult result ->
            -- TODO: redirect on succes
            -- TODO: show error on failure
            model ! []


validModel : Model -> Bool
validModel model =
    let
        questionValid =
            model.question /= ""

        choicesValid =
            List.length (validChoices model) >= 2
    in
        questionValid && choicesValid


validChoices : Model -> List String
validChoices model =
    Dict.values model.choices
        |> List.filter (\text -> text /= "")


changeText : Int -> String -> Model -> Model
changeText index text model =
    { model | choices = Dict.insert index text model.choices }
        |> removeEmptyChoices
        |> addEmptyChoice


removeEmptyChoices : Model -> Model
removeEmptyChoices model =
    let
        newChoices =
            Dict.filter (\_ text -> text /= "") model.choices
    in
        { model | choices = newChoices }


addEmptyChoice : Model -> Model
addEmptyChoice model =
    let
        index =
            (List.maximum (Dict.keys model.choices)
                |> Maybe.withDefault 0
            )
                + 1
    in
        { model | choices = Dict.insert index "" model.choices }


view : Model -> Html Msg
view model =
    div []
        [ form model
        ]


form : Model -> Html Msg
form model =
    Html.form
        [ Attr.class "form-horizontal"
        , Ev.onSubmit NoOp
        ]
        ([ div [ Attr.class "form-group" ]
            [ label
                [ Attr.for "inputQuesstion"
                , Attr.class "col-sm-2 control-label"
                ]
                [ text "Question" ]
            , div [ Attr.class "col-sm-10" ]
                [ input
                    [ Attr.type_ "text"
                    , Attr.class "form-control"
                    , Attr.id "inputQuestion"
                    , Attr.placeholder "Question..."
                    , Ev.onInput EditQuestion
                    ]
                    []
                ]
            ]
         ]
            ++ viewChoices model
            ++ [ div [ Attr.class "form-group" ]
                    [ div [ Attr.class "col-sm-offset-2 col-sm-10" ]
                        [ button
                            [ Attr.type_ "submit"
                            , Attr.class "btn btn-default"
                            , Attr.disabled (not <| validModel model)
                            , Ev.onClick Execute
                            ]
                            [ text "create" ]
                        ]
                    ]
               ]
        )


viewChoices : Model -> List (Html Msg)
viewChoices model =
    Dict.toList model.choices
        |> List.indexedMap (viewChoice model)


viewChoice : Model -> Int -> ( Int, String ) -> Html Msg
viewChoice model index ( key, txt ) =
    div [ Attr.class "form-group" ]
        [ label [ Attr.class "col-sm-2 control-label" ]
            [ text
                (if index == 0 then
                    "Choices:"
                 else
                    ""
                )
            ]
        , div [ Attr.class "col-sm-10" ]
            [ input
                [ Attr.type_ "text"
                , Attr.class "form-control"
                , Attr.placeholder "Choice.."
                , Attr.value txt
                , Ev.onInput (EditChoice key)
                ]
                []
            ]
        ]
