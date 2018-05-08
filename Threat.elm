module Threat exposing (update, validateThreat)

import Types exposing (..)


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


update : Msg -> Threat -> Result ( Threat, String ) Threat
update ef threat =
    {- TODO: should update the threat with new data, run validation? -}
    --Ok threat
    case ef of
        EditMsg threatfieldid text ->
            updateField threatfieldid text threat

        _ ->
            Err ( threat, "Bad message to threat.update" )
