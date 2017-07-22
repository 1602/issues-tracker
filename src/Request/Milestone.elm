module Request.Milestone exposing (create, list)

import Http exposing (Error, Response)
import Messages exposing (Msg(..))
import Data.Milestone as Milestone exposing (Milestone)
import Json.Encode as Encode
import Models exposing (Model)
import Request.Helpers exposing (cachingFetch, apiUrl, withAuthorization)
import HttpBuilder exposing (withExpect, withBody)
import Util exposing ((=>))


create : String -> String -> String -> Cmd Msg
create repo title accessToken =
    let
        body =
            [ "title" => Encode.string title
            ]
                |> Encode.object
                |> Http.jsonBody
    in
        apiUrl ("/repos/" ++ repo ++ "/milestones")
            |> HttpBuilder.post
            |> withAuthorization accessToken
            |> withExpect (Http.expectJson Milestone.decoder)
            |> withBody body
            |> HttpBuilder.toRequest
            |> Http.send MilestoneCreated


list : Model -> Cmd Msg
list model =
    cachingFetch
        (apiUrl <| "/repos/" ++ model.repo ++ "/milestones")
        model.persistentData.accessToken
        model.etags
        LoadMilestones
