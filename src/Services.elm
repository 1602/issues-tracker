module Services exposing (..)

import Json.Decode as Decode
import Base64
import Http exposing (Error, Response)
import Messages exposing (..)
import Decoders exposing (..)
import Models exposing (..)


-- import Json.Encode as Encode
-- import Base exposing (..)


env : String
env =
    -- "staging"
    -- "sandbox"
    "beta"


repo : String
repo =
    "universalbasket/engineering"


authHeader : String -> Http.Header
authHeader secretKey =
    Http.header "Authorization" <|
        "Basic "
            ++ (secretKey ++ ":" |> Base64.encode |> Result.withDefault "")


fetchIssues : String -> Column -> Cmd Msg
fetchIssues accessToken column =
    let
        milestone =
            case column of
                Icebox ->
                    "&milestone=none"

                _ ->
                    "&milestone=*"

        labels =
            case column of
                Icebox ->
                    ""

                Backlog ->
                    "&labels=Status: Ready"

                Current ->
                    "&labels=Status: In Progress"

                Done ->
                    ""

        state =
            case column of
                Done ->
                    "&state=closed"

                _ ->
                    ""
    in
        Http.request
            { method = "GET"
            , headers = []
            , url =
                "https://api.github.com/repos/"
                    ++ repo
                    ++ "/issues?access_token="
                    ++ accessToken
                    ++ labels
                    ++ state
                    ++ milestone
            , expect = Http.expectJson <| Decode.at [] <| Decode.list issueDecoder
            , body = Http.emptyBody
            , timeout = Nothing
            , withCredentials = False
            }
            |> Http.send (IssuesLoaded column)
