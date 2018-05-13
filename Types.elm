module Types exposing (..)

import Threat

import Http exposing (Error)
--import Json.Decode
--import Json.Decode.Pipeline
import Json.Encode


type alias Model =
    { threats : List Threat.Threat, status : String, viewModel : ViewModel }


type alias ViewModel =
    { state : String }


modelEncoder : Model -> Json.Encode.Value
modelEncoder model =
    let
        threatsjson =
            List.map Threat.threatEncoder model.threats
    in
    Json.Encode.object
        [ ( "threats", Json.Encode.list threatsjson )
        , ( "status", Json.Encode.string model.status )

        --DON'T ENCODE View State (in ViewModel)
        ]


type GenerateType
    = CSV
    | JSON


type alias JsonData =
    List Threat.Threat


type Msg
    = Generate GenerateType
    | ResetSelections
    | EditMsg Threat.ThreatFieldId String
    | DataReceived (Result Http.Error JsonData)
    | NewThreatMsg Model

