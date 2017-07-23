module Request.Repo exposing (list)

import HttpBuilder
import Request.Helpers exposing (withAuthorization, apiUrl)
import Request.Cache exposing (withCache, RemoteData, Etags)
import Http
import Data.Repo as Repo exposing (Repo)
import Json.Decode as Decode

list : String -> Etags -> Http.Request (RemoteData (List Repo))
list accessToken etags =
    let
        decoder =
            Decode.list Repo.decoder
    in
        apiUrl "/user/repos?sort=updated"
            |> HttpBuilder.get
            |> withAuthorization accessToken
            |> withCache etags decoder
            |> HttpBuilder.toRequest
