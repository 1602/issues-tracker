module Request.Repo exposing (list)

import Request.Helpers exposing (withAuthorization, apiUrl)
import Http
import HttpBuilder
import Data.Repo as Repo exposing (Repo)
import Json.Decode as Decode

list : String -> Http.Request (List Repo)
list accessToken =
    let
        expect =
            Http.expectJson (Decode.list Repo.decoder)
    in
        apiUrl "/user/repos?sort=updated"
            |> HttpBuilder.get
            |> HttpBuilder.withExpect expect
            |> withAuthorization accessToken
            |> HttpBuilder.toRequest
