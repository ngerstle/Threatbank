module Types exposing (..)

import Http exposing (Error)
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type alias Title =
    String


type alias Description =
    String


type Severity
    = Low
    | Medium
    | High


severitylist : List Severity
severitylist =
    [ Low, Medium, High ]


severityDecoder : Json.Decode.Decoder Severity
severityDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "Low" ->
                        Json.Decode.succeed Low

                    "Medium" ->
                        Json.Decode.succeed Medium

                    "High" ->
                        Json.Decode.succeed High

                    _ ->
                        Json.Decode.fail <| "Unknown category " ++ str
            )



-- convert to cvss, add a validatecvss: Severity -> Maybe Errormessage, etc
-- validatecvss: severity -> Result String _?


type Category
    = Spoofing
    | Tampering
    | Repudiation
    | InformationDisclosure
    | DoS
    | ElevationOfPrivilege
    | Miscellaneous


categorylist : List Category
categorylist =
    [ Spoofing
    , Tampering
    , Repudiation
    , InformationDisclosure
    , DoS
    , ElevationOfPrivilege
    , Miscellaneous
    ]


categoryDecoder : Json.Decode.Decoder Category
categoryDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "Spoofing" ->
                        Json.Decode.succeed Spoofing

                    "Tampering" ->
                        Json.Decode.succeed Tampering

                    "Repudiation" ->
                        Json.Decode.succeed Repudiation

                    "Information Disclosure" ->
                        Json.Decode.succeed InformationDisclosure

                    "InformationDisclosure" ->
                        Json.Decode.succeed InformationDisclosure

                    "Denial of Service" ->
                        Json.Decode.succeed DoS

                    "DoS" ->
                        Json.Decode.succeed DoS

                    "Elevation of Privilege" ->
                        Json.Decode.succeed ElevationOfPrivilege

                    "ElevationOfPrivilege" ->
                        Json.Decode.succeed ElevationOfPrivilege

                    "Miscellaneous" ->
                        Json.Decode.succeed Miscellaneous

                    _ ->
                        Json.Decode.fail <| "Unknown category " ++ str
            )


type alias Remediation =
    String



-- type RiskAcceptance = False | True String -- add type about risk accepted (with True requiring name of acceptor


type alias ID =
    Int


type alias Selected =
    Bool


type alias Threat =
    { id : ID, title : Title, description : Description, remediation : Remediation, severity : Severity, category : Category, selected : Selected }


threatDecoder : Json.Decode.Decoder Threat
threatDecoder =
    Json.Decode.Pipeline.decode Threat
        |> Json.Decode.Pipeline.optional "Id" Json.Decode.int -1
        |> Json.Decode.Pipeline.required "Title" Json.Decode.string
        |> Json.Decode.Pipeline.required "Description" Json.Decode.string
        |> Json.Decode.Pipeline.required "Remediation" Json.Decode.string
        |> Json.Decode.Pipeline.required "Severity" severityDecoder
        |> Json.Decode.Pipeline.required "Category" categoryDecoder
        |> Json.Decode.Pipeline.optional "Selected" Json.Decode.bool False


threatEncoder : Threat -> Json.Encode.Value
threatEncoder threat =
    Json.Encode.object
        [ ( "Id", Json.Encode.int threat.id )
        , ( "Title", Json.Encode.string threat.title )
        , ( "Description", Json.Encode.string threat.description )
        , ( "Remediation", Json.Encode.string threat.remediation )
        , ( "Severity", Json.Encode.string (toString threat.severity) )
        , ( "Category", Json.Encode.string (toString threat.severity) )
        , ( "Selected", Json.Encode.bool threat.selected )
        ]


type alias Model =
    { threats : List Threat, status : String }


modelEncoder : Model -> Json.Encode.Value
modelEncoder model =
    let
        threatsjson =
            List.map threatEncoder model.threats
    in
    Json.Encode.object
        [ ( "threats", Json.Encode.list threatsjson )
        , ( "status", Json.Encode.string model.status )
        ]


type GenerateType
    = CSV
    | JSON


type alias JsonData =
    List Threat


type Msg
    = Generate GenerateType
    | ResetSelections
    | EditMsg ThreatFieldId String
    | DeleteMsg ThreatFieldId
    | DataReceived (Result Http.Error JsonData)
    | NewThreatMsg Model


type alias ThreatFieldId =
    { id : ID, field : ThreatField }



--{-- TODO (instead of random strings..)


type ThreatField
    = ID
    | Title
    | Description
    | Severity
    | Category
    | Remediation
    | Selected
--}
