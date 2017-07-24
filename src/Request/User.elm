module Request.User exposing (get)

import Base64
import Http exposing (Error, Response)
import Dict
import Data.User as User exposing (User)
import Json.Decode as Decode
import HttpBuilder exposing (withHeader)
import Request.Helpers exposing (withAuthorization)


get : String -> Http.Request User
get accessToken =
    let
        expect =
            Http.expectStringResponse
                (\res ->
                    let
                        hasRepoOauthScope =
                            res.headers
                                |> Dict.toList
                                |> List.map (\( key, value ) -> ( String.toLower key, value ))
                                |> Dict.fromList
                                |> Dict.get "x-oauth-scopes"
                                |> Maybe.withDefault ""
                                |> String.split ", "
                                |> List.member "repo"
                    in
                        if hasRepoOauthScope then
                            Decode.decodeString User.decoder res.body
                        else
                            Err "Insufficient permissions: 'repo' oauth scope is required"
                )
    in
        "https://api.github.com/user"
            |> HttpBuilder.get
            |> HttpBuilder.withExpect expect
            |> withAuthorization accessToken
            |> withHeader "if-modified-since" "0"
            |> HttpBuilder.toRequest


buildAuthHeader : String -> String
buildAuthHeader secretKey =
    secretKey
        ++ ":"
        |> Base64.encode
        |> Result.withDefault ""
        |> (++) "Basic "
