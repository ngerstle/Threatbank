module Main exposing (..)

import Base64
import Html
import Json.Encode
import List.Extra
import Navigation
import Rest exposing (..)
import Threat exposing (..)
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


getSubThreat : Model -> ThreatFieldId -> Maybe Threat
getSubThreat model threatfieldid =
    {- Gets a threat based on an EditField event -}
    let
        --(id,_) = ef
        threats =
            List.filter (\x -> x.id == threatfieldid.id) model.threats
    in
    List.head threats


updateSubThreat : Model -> Result ( Threat, String ) Threat -> Model
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


deleteThreat : ThreatFieldId -> Model -> Model
deleteThreat threatfieldid model =
    let
        otherthreats =
            List.filter (\x -> x.id /= threatfieldid.id) model.threats

        status =
            "deleted threat id=(" ++ toString threatfieldid.id ++ ")"
    in
    { model | threats = otherthreats, status = status }


updateThreat : ThreatFieldId -> Msg -> Model -> Model
updateThreat threatid msg model =
    let
        threatToUpdate =
            getSubThreat model threatid
    in
    case threatToUpdate of
        Just actualthreat ->
            updateSubThreat model (Threat.update msg actualthreat)

        Nothing ->
            { model | status = "no threat by that id" }


setid : List Threat -> List Threat
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
                -- cmd = Cmd.none
                activeThreats =
                    List.filter .selected model.threats

                rawpayload =
                    exportThreatsCSV activeThreats

                payload64 =
                    Base64.encode rawpayload

                cmd =
                    Navigation.load ("data:application/csv;base64," ++ payload64)
            in
            ( { model | status = "generate CSV file" }, cmd )

        Generate JSON ->
            --TODO redirect to dataurl with json
            let
                -- cmd = Cmd.none
                rawpayload =
                    Json.Encode.encode 2 (modelEncoder model)

                payload64 =
                    Base64.encode rawpayload

                cmd =
                    Navigation.load ("data:application/json;base64," ++ payload64)
            in
            ( { model | status = "generate JSON" }, cmd )

        EditMsg threatfieldId txt ->
            --{ model | status = "update threats" }
            ( updateThreat threatfieldId msg model, Cmd.none )

        DeleteMsg threatfieldId ->
            ( deleteThreat threatfieldId model, Cmd.none )

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
