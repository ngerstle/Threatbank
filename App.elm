module Main exposing (..)

-- exposing (..)

import Base64
import Html
import Json.Encode
import List.Extra
import Navigation
import Rest exposing (..)
import Threat
import Types exposing (..)
import View exposing (..)


init : ( Model, Cmd Msg )
init =
    let
        threats =
            loadThreats "data"

        iviewmodel =
            ViewModel ""

        imodel =
            Model threats "initial model, predataload" iviewmodel
    in
    ( imodel, getJsonData )


validateModel : Model -> Result String Model
validateModel m =
    let
        threatsValid =
            List.map Threat.validateThreat m.threats

        filterValidThreats res =
            case res of
                Ok _ ->
                    False

                Err _ ->
                    True

        invalidThreats =
            List.filter filterValidThreats threatsValid

        lenInvalid =
            List.length invalidThreats

        threatIDs =
            List.map .id m.threats

        uniqIds =
            List.Extra.allDifferent threatIDs
    in
    if lenInvalid == 0 && uniqIds then
        Ok m
    else if not uniqIds then
        Err "Threat IDs not unique"
    else
        Err (toString lenInvalid ++ " Invalid Threats: " ++ String.concat (List.map toString invalidThreats))



-- could add which threats are invalid and why


updateSubThreat : Model -> Result ( Threat.Threat, String ) Threat.Threat -> Model
updateSubThreat model threatresult =
    {- Replaces an existing Threat with an updated Threat -}
    let
        ( threat, status ) =
            case threatresult of
                Ok t ->
                    ( t, "updated threat" )

                Err ( t, s ) ->
                    ( t, s )

        otherthreats =
            List.filter (\x -> x.id /= threat.id) model.threats

        newthreats =
            List.sortBy .id (threat :: otherthreats)
    in
    { model | threats = newthreats, status = status }


updateThreat : Threat.ThreatFieldId -> String -> Model -> Model
updateThreat threatid text model =
    let
        getSubThreat : Model -> Threat.ThreatFieldId -> Maybe Threat.Threat
        getSubThreat model threatfieldid =
            {- Gets a threat based on an EditField event -}
            let
                threats =
                    List.filter (\x -> x.id == threatfieldid.id) model.threats
            in
            List.head threats

        threatToUpdate =
            getSubThreat model threatid
    in
    case threatToUpdate of
        Just actualthreat ->
            updateSubThreat model (Threat.updateField threatid text actualthreat)

        Nothing ->
            { model | status = "no threat by that id" }


setid : List Threat.Threat -> List Threat.Threat
setid threatList =
    let
        maxid =
            Maybe.withDefault -1 (List.maximum (List.map .id threatList))

        threatsNoId =
            List.filter (\x -> x.id < 0) threatList

        threatsId =
            List.filter (\x -> x.id >= 0) threatList

        newIds =
            List.range (1 + maxid) (maxid + List.length threatsNoId)

        newIdThreats =
            List.map2 (\a b -> { a | id = b }) threatsNoId newIds

        newThreatList =
            newIdThreats ++ threatsId
    in
    newThreatList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate CSV ->
            let
                activeThreats =
                    List.filter .selected model.threats

                rawpayload =
                    Threat.exportThreatsCSV activeThreats

                payload64 =
                    Base64.encode rawpayload

                cmd =
                    Navigation.load ("data:application/csv;base64," ++ payload64)
            in
            ( { model | status = "generate CSV file" }, cmd )

        Generate JSON ->
            let
                rawpayload =
                    Json.Encode.encode 2 (modelEncoder model)

                payload64 =
                    Base64.encode rawpayload

                cmd =
                    Navigation.load ("data:application/json;base64," ++ payload64)
            in
            ( { model | status = "generate JSON" }, cmd )

        EditMsg threatfieldId txt ->
            ( updateThreat threatfieldId txt model, Cmd.none )

        DataReceived (Ok threats) ->
            let
                idthreats =
                    setid threats

                newModel =
                    { model | threats = idthreats }
            in
            case validateModel newModel of
                Ok vNewModel ->
                    ( { newModel | status = "datafetched, loaded." }, Cmd.none )

                Err errstring ->
                    ( { model | status = "datafetched, but " ++ errstring }, Cmd.none )

        DataReceived (Err httpError) ->
            ( { model
                | status = "httperror " ++ createErrorMessage httpError
              }
            , Cmd.none
            )

        ResetSelections ->
            let
                newmodel =
                    { model
                        | threats = List.map (\t -> { t | selected = False }) model.threats
                    }
            in
            ( { newmodel
                | status = "threat selections cleared.."
              }
            , Cmd.none
            )

        NewThreatMsg _ ->
            ( { model | status = "new threat msg not implemented" }, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = View.view
        , subscriptions = \_ -> Sub.none
        }
