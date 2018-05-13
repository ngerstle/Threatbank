module Rest exposing (createErrorMessage, getJsonData, loadThreats)

import Http
import Json.Decode
import Threat
import Types exposing (..)


-- exposing (..)


threatbankurl : String
threatbankurl =
    "threatbank_json.json"


loadThreats : String -> List Threat.Threat
loadThreats url =
    let
        testthreat1 =
            Threat.Threat 1 "ab" "cd" "ef" Threat.Low Threat.Spoofing True

        testthreat2 =
            Threat.Threat 2 "ab" "cd" "ef" Threat.Low Threat.Spoofing True

        testthreat3 =
            Threat.Threat 3 "ab" "cd" "ef" Threat.Low Threat.Spoofing True
    in
    [ testthreat1, testthreat2, testthreat3 ]


decodeRequestJson : Model -> String -> ( Model, Cmd Msg )
decodeRequestJson model rawjson =
    ( model, Cmd.none )


getJsonData : Cmd Msg
getJsonData =
    let
        jsonDecoder : Json.Decode.Decoder (List Threat.Threat)
        jsonDecoder =
            Json.Decode.list Threat.threatDecoder

        jsonRequest : Http.Request (List Threat.Threat)
        jsonRequest =
            Http.get threatbankurl jsonDecoder
    in
    Http.send DataReceived jsonRequest


createErrorMessage : Http.Error -> String
createErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "It appears you don't have an Internet connection right now."

        Http.BadStatus response ->
            response.status.message

        Http.BadPayload message response ->
            message
