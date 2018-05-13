module View exposing (view)

import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Types exposing (..)
import Threat exposing (..)


--import Threat exposing (..)


generateButton : Msg -> String -> Html.Html Msg
generateButton msg text =
    Html.div [ Html.Attributes.class "GenerateButton" ]
        [ Html.button
            [ Html.Attributes.class "mdl-button mdl-js-button mdl-button--raised"
            , Html.Events.onClick msg
            ]
            [ Html.text text ]
        ]


view : Model -> Html.Html Msg
view m =
    let
        viewThreatsList =
            m.threats

        subdivs =
            List.map viewThreatCard viewThreatsList
    in
    Html.div []
        [ Html.h1 [ Html.Attributes.class "grow" ] [ Html.text "THREAT BANK" ]
        , Html.div [ Html.Attributes.class "ThreatTable" ] subdivs
        , Html.br [] []
        , Html.br [] []

        --TODO , Html.div [addRow] -- option to add a threat
        , newThreat
        , viewButtons
        , Html.text ("Status: >>" ++ m.status ++ "<<")
        ]


newThreat : Html.Html Msg
newThreat =
    Html.div []
        [ Html.text "New threat field here"
        ]


viewButtons : Html.Html Msg
viewButtons =
    Html.div []
        [ generateButton (Generate CSV) "Generate CSV"
        , generateButton (Generate JSON) "Generate JSON"
        , generateButton ResetSelections "Reset Selections"
        ]


onBlurWithTargetValue : (String -> msg) -> Html.Attribute msg
onBlurWithTargetValue tagger =
    {- A variant of onBlur that includes the value of the text field just blurred -}
    Html.Events.on "blur" (Json.Decode.map tagger Html.Events.targetValue)


textField :
    ID
    -> ThreatField
    -> String
    -> Html.Html Msg
textField id fieldname text =
    let
        sfieldname =
            toString fieldname

        tfid =
            ThreatFieldId id fieldname

        msg =
            EditMsg tfid

        --TODO check validity, & add visual indicator flags if not (here)... (using css?)
    in
    Html.div [ Html.Attributes.class "ThreatField" ]
        [ Html.text (sfieldname ++ ": ")
        , Html.input
            [ Html.Attributes.id (toString id ++ "." ++ sfieldname)
            , Html.Attributes.type_ sfieldname
            , Html.Attributes.value text

            --            , Html.Events.onBlur msg
            , onBlurWithTargetValue msg
            ]
            []
        ]


cssThreatButton : String
cssThreatButton =
    "DeleteThreatButton"


cssThreatFields : String
cssThreatFields =
    "ThreatFields"


cssThreatId : String
cssThreatId =
    "ThreatID"


cssThreatRow : String
cssThreatRow =
    "ThreatRow"


cssThreatCard : String
cssThreatCard =
    "demo-card-event mdl-card mdl-shadow--2dp"


cssThreatCardTop : String
cssThreatCardTop =
    "mdl-card__title mdl-card--expand"


cssThreatCardCheckbox : String
cssThreatCardCheckbox =
    "mdl-checkbox mdl-js-checkbox mdl-js-ripple-effect"


cssThreatCardCheckboxInput : String
cssThreatCardCheckboxInput =
    "mdl-checkbox__input"


cssThreatCardDescription : String
cssThreatCardDescription =
    "mdl-card__actions mdl-card--border"


cssThreatCardSpacer : String
cssThreatCardSpacer =
    "mdl-layout-spacer"


viewThreatCard : Threat -> Html.Html Msg
viewThreatCard threat =
    let
        idtextfield =
            textField threat.id

        iddropdown =
            dropDown threat.id

        selectedMsg : Msg
        selectedMsg =
            EditMsg (ThreatFieldId threat.id Selected) ""
    in
    Html.div []
        [ Html.br [] []
        , Html.br [] []
        , Html.div [ Html.Attributes.class cssThreatCard ]
            [ Html.div [ Html.Attributes.class cssThreatCardTop ]
                [ Html.label
                    [ Html.Attributes.class cssThreatCardCheckbox
                    , Html.Attributes.for ("checkbox-" ++ toString threat.id)
                    ]
                    [ Html.input
                        [ Html.Attributes.type_ "checkbox"
                        , Html.Attributes.id ("checkbox-" ++ toString threat.id)
                        , Html.Attributes.class cssThreatCardCheckboxInput
                        , Html.Attributes.checked threat.selected
                        , Html.Events.onClick selectedMsg
                        ]
                        []
                    ]
                , Html.h4 [] [ Html.text threat.title ]
                ]
            , Html.div [ Html.Attributes.class cssThreatCardDescription ]
                [ Html.h5 [] [ Html.text threat.description ]
                , Html.div [ Html.Attributes.class cssThreatCardSpacer ] []
                ]
            ]
        ]


dropDown :
    ID
    -> ThreatField
    -> String
    -> List String
    -> Html.Html Msg
dropDown id currentvalue fieldname values =
    let
        sfieldname =
            toString fieldname
    in
    {- TODO drop down elements for enum value fields -}
    Html.div [ Html.Attributes.class "ThreatField" ]
        [ Html.text (sfieldname ++ ": ")

        --, Html.text "NOT IMPLEMENTED"
        , Html.text (toString currentvalue)
        ]
