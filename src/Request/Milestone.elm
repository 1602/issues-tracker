module Request.Milestone exposing (create, list)

import Http exposing (Error, Response)
import Messages exposing (Msg(..))
import Data.Milestone as Milestone exposing (Milestone)
import Json.Encode as Encode
import Models exposing (Model)
import Request.Helpers exposing (apiUrl, withAuthorization)
import Request.Cache exposing (cachingFetch)
import HttpBuilder exposing (withExpect, withBody)
import Util exposing ((=>))


create : (String, String) -> String -> String -> Cmd Msg
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
            |> Http.send MilestoneCreated


list : Model -> Cmd Msg
list { repo, persistentData, etags } =
    let
        (u, r) =
            repo

        url =
            apiUrl <| "/repos/" ++ u ++ "/" ++ r ++ "/milestones"
    in
        cachingFetch
            url
            persistentData.accessToken
            etags
            LoadMilestones
