module Request.Milestone exposing (create, list)

import Http exposing (Error, Response)
import Messages exposing (Msg(..))
import Data.Milestone as Milestone exposing (Milestone)
import Json.Encode as Encode
import Models exposing (Model)
import Request.Helpers exposing (cachingFetch)

create : String -> String -> Maybe String -> Cmd Msg
create repo title accessToken =
    Http.request
        { method = "POST"
        , headers = []
        , url =
            "https://api.github.com/repos/" ++ repo ++ "/milestones"
            ++ "?access_token=" ++ (Maybe.withDefault "" accessToken)
        , expect = Http.expectJson <| Milestone.decoder
        , body = Http.jsonBody <|
            Encode.object
                [ ( "title", Encode.string title )
                ]
        , timeout = Nothing
        , withCredentials = False
        }
            |> Http.send MilestoneCreated



list : Model -> Cmd Msg
list model =
    let
        url =
            "https://api.github.com/repos/"
                ++ model.repo
                ++ "/milestones"
    in
        cachingFetch
            url
            (Maybe.withDefault "" model.accessToken)
            model.etags
            LoadMilestones
