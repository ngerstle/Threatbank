module Types exposing (..)

import Http exposing (Error)
import Json.Encode
import Threat


type alias Model =
    { threats : List Threat.Threat, status : String, viewModel : ViewModel }


type alias ViewModel =
    { newThreatTitle : String, newThreatDescription : String }


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
    | NewThreatMsg Threat.ThreatField String
    | AddNewThreatMsg
