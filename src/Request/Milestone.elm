module Request.Milestone exposing (create, list)

import Http exposing (Error, Response, Request)
import Data.Milestone as Milestone exposing (Milestone)
import Json.Encode as Encode
import Json.Decode as Decode
import Request.Helpers exposing (apiUrl, withAuthorization)
import Request.Cache exposing (withCache, CachedRequest, Etags)
import HttpBuilder exposing (withExpect, withBody)
import Util exposing ((=>))


create : (String, String) -> String -> String -> Request Milestone
create (user, repo) title accessToken =
    let
        body =
            [ "title" => Encode.string title
            ]
                |> Encode.object
                |> Http.jsonBody
    in
        apiUrl ("/repos/" ++ user ++ "/" ++ repo ++ "/milestones")
            |> HttpBuilder.post
            |> withAuthorization accessToken
            |> withExpect (Http.expectJson Milestone.decoder)
            |> withBody body
            |> HttpBuilder.toRequest


list : (String, String) -> String -> Etags ->  CachedRequest (List Milestone)
list repo accessToken etags =
    let
        (u, r) =
            repo

        decoder =
            Decode.list Milestone.decoder
    in
        apiUrl ("/repos/" ++ u ++ "/" ++ r ++ "/milestones")
            |> HttpBuilder.get
            |> withAuthorization accessToken
            |> withCache etags decoder
            |> HttpBuilder.toRequest
