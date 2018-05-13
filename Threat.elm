--module Threat exposing (exportThreatsCSV, updateField, validateThreat, Threat, ThreatFieldId, threatEncoder)


module Threat exposing (..)

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


type alias ThreatFieldId =
    { id : ID, field : ThreatField }


type ThreatField
    = ID
    | Title
    | Description
    | Severity
    | Category
    | Remediation
    | Selected


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


validateID : ID -> Result String ID
validateID id =
    Ok id


maxTitleLength : Int
maxTitleLength =
    60


validateTitle : Title -> Result String Title
validateTitle title =
    let
        titlelength =
            String.length title
    in
    if titlelength == 0 then
        Err "Missing title"
    else if titlelength > maxTitleLength then
        Err ("Title too long (>" ++ toString maxTitleLength ++ ")")
    else
        Ok title


validateDescription : Description -> Result String Description
validateDescription description =
    let
        descriptionlength =
            String.length description
    in
    if descriptionlength == 0 then
        Err "Missing description"
    else if descriptionlength < 10 then
        Err "Description suspiciously short (<10)"
    else
        Ok description


validateRemediation : Remediation -> Result String Remediation
validateRemediation remediation =
    let
        remediationlength =
            String.length remediation
    in
    if remediationlength == 0 then
        Err "Missing remediation"
    else if remediationlength < 10 then
        Err "Remediation suspiciously short (<10)"
    else
        Ok remediation


validateSeverity : Severity -> Result String Severity
validateSeverity severity =
    Ok severity


validateCategory : Category -> Result String Category
validateCategory category =
    Ok category


validateThreat : Threat -> Result (List String) Threat
validateThreat threat =
    let
        idErr =
            validateID threat.id

        titleErr =
            validateTitle threat.title

        descriptionErr =
            validateDescription threat.description

        severityErr =
            validateSeverity threat.severity

        categoryErr =
            validateCategory threat.category

        remediationErr =
            validateRemediation threat.remediation

        getErr res =
            case res of
                Err msg ->
                    [ msg ]

                Ok _ ->
                    []

        errorlist =
            [ getErr idErr
            , getErr titleErr
            , getErr descriptionErr
            , getErr severityErr
            , getErr categoryErr
            , getErr remediationErr
            ]

        filteredList =
            List.foldr (++) [] errorlist
    in
    if List.isEmpty filteredList then
        Ok threat
    else
        Err filteredList


updateDropdownField : ThreatField -> String -> Threat -> Result ( Threat, String ) Threat
updateDropdownField threatfield text threat =
    {- TODO
       newval = case threatfield of
                    Category -> stringToCategory text
                    Severity -> stringToSeverity text
    -}
    let
        status =
            "not implemented"
    in
    Err ( threat, status )


updateField : ThreatFieldId -> String -> Threat -> Result ( Threat, String ) Threat
updateField threatfieldid text threat =
    if threat.id /= threatfieldid.id then
        Err ( threat, text )
    else
        case threatfieldid.field of
            Title ->
                Ok { threat | title = text }

            Description ->
                Ok { threat | description = text }

            Remediation ->
                Ok { threat | remediation = text }

            Severity ->
                updateDropdownField Severity text threat

            Category ->
                updateDropdownField Category text threat

            ID ->
                Err ( threat, "Can't change a threat ID" )

            Selected ->
                Ok { threat | selected = not threat.selected }



--       _ ->
--           Err ( threat, "Unknown category to update in threat.updateField" )


exportThreatsCSV : List Threat -> String
exportThreatsCSV threats =
    --TODO do proper escaping on Title/Description/Remediation
    let
        headers =
            "Id,Title,Description,Remediation,Severity,Category,Selected"

        threatToCsv : Threat -> String
        threatToCsv threat =
            String.join ","
                [ toString threat.id
                , toString threat.title
                , toString threat.description
                , toString threat.remediation
                , toString threat.severity
                , toString threat.category
                , toString threat.selected
                ]
    in
    String.join "\n"
        (headers
            :: List.map threatToCsv threats
        )
