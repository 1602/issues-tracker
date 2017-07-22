module Request.User exposing (get)

import Http exposing (Error, Response)
import Messages exposing (Msg(..))
import Dict
import Data.User as User exposing (User)
import Json.Decode as Decode

get : String -> Cmd Msg
get accessToken =
    Http.request
        { method = "GET"
        , headers = [ Http.header "If-Modified-Since" "0"]
        , url =
            "https://api.github.com/user?access_token="
                ++ accessToken
        , expect = Http.expectStringResponse (\res ->
            let
                hasRepoOauthScope =
                    res.headers
                        |> Dict.toList
                        |> List.map (\(key, value) -> (String.toLower key, value))
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
        , body = Http.emptyBody
        , timeout = Nothing
        , withCredentials = False
        }
            |> Http.send LoadUser
