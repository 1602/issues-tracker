module Services exposing (..)

import Json.Decode as Decode
import Base64
-- import Task
import Http exposing (Error, Response)
import Messages exposing (..)
import Decoders exposing (..)
import Models exposing (..)
import Json.Encode as Encode
import Dict


-- import Json.Encode as Encode
-- import Base exposing (..)

env : String
env =
    -- "staging"
    -- "sandbox"
    "beta"


authHeader : String -> Http.Header
authHeader secretKey =
    Http.header "Authorization" <|
        "Basic "
            ++ (secretKey ++ ":" |> Base64.encode |> Result.withDefault "")

        {-
fetchBacklog : String -> Cmd Msg
fetchBacklog accessToken =
    let
        fetchIssuesForMilestones milestones =
            milestones
                |> List.map fetchIssuesForMilestone
                |> Task.sequence

        fetchIssuesForMilestone milestone =
            Http.request
                { method = "GET"
                , headers = []
                , url =
                    "https://api.github.com/repos/"
                        ++ repo
                        ++ "/issues?access_token="
                        ++ accessToken
                        ++ "&milestone="
                        ++ milestone.number
                , expect = Http.expectJson <| Decode.list issueDecoder
                , body = Http.emptyBody
                , timeout = Nothing
                , withCredentials = False
                }
                    |> Http.toTask

        fetchMilestones =
            Http.request
                { method = "GET"
                , headers = []
                , url =
                    "https://api.github.com/repos/"
                        ++ repo
                        ++ "/milestones?access_token="
                        ++ accessToken
                , expect = Http.expectJson <| Decode.list milestoneDecoder
                , body = Http.emptyBody
                , timeout = Nothing
                , withCredentials = False
                }
                    |> Http.toTask
    in
        Task.attempt LoadMilestones (fetchMilestones
            |> Task.andThen fetchIssuesForMilestones)
        -}

createMilestone : String -> String -> Maybe String -> Cmd Msg
createMilestone repo title accessToken =
    Http.request
        { method = "POST"
        , headers = []
        , url =
            "https://api.github.com/repos/" ++ repo ++ "/milestones"
            ++ "?access_token=" ++ (Maybe.withDefault "" accessToken)
        , expect = Http.expectJson <| milestoneDecoder
        , body = Http.jsonBody <|
            Encode.object
                [ ( "title", Encode.string title )
                ]
        , timeout = Nothing
        , withCredentials = False
        }
            |> Http.send MilestoneCreated

fetchMilestoneIssues : String -> String -> IssueState -> Milestone -> Cmd Msg
fetchMilestoneIssues repo accessToken issueState ms =
    let
        state =
            case issueState of
                IssueOpen ->
                    "&state=open"

                IssueClosed ->
                    "&state=closed"
    in
        Http.request
            { method = "GET"
            , headers = [ Http.header "If-Modified-Since" "0"]
            , url =
                "https://api.github.com/repos/"
                    ++ repo
                    ++ "/issues?access_token="
                    ++ accessToken
                    ++ state
                    ++ "&milestone=" ++ ms.number
            , expect = Http.expectJson <| Decode.at [] <| Decode.list issueDecoder
            , body = Http.emptyBody
            , timeout = Nothing
            , withCredentials = False
            }
                |> Http.send (MilestoneIssuesLoaded ms.number issueState)

fetchIssues : String -> String -> Column -> Cmd Msg
fetchIssues repo accessToken column =
    let
        milestone =
            case column of
                Icebox ->
                    "&milestone=none"

                _ ->
                    ""

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
            , headers = [ Http.header "If-Modified-Since" "0"]
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

updateIssueWith : String -> String -> Decode.Value -> String -> (Result Error Issue -> a) -> Cmd a
updateIssueWith repo issueNumber issue accessToken onComplete =
    Http.request
        { method = "PATCH"
        , headers = []
        , url =
            "https://api.github.com/repos/"
                ++ repo
                ++ "/issues/"
                ++ issueNumber
                ++ "?access_token="
                ++ accessToken
        , expect = Http.expectJson issueDecoder
        , body = Http.jsonBody issue
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send onComplete

updateIssue : String -> Issue -> String -> (Result Error Issue -> a) -> Cmd a
updateIssue repo issue accessToken onComplete =
    Http.request
        { method = "PATCH"
        , headers = []
        , url =
            "https://api.github.com/repos/"
                ++ repo
                ++ "/issues/"
                ++ issue.number
                ++ "?access_token="
                ++ accessToken
        , expect = Http.expectJson issueDecoder
        , body = Http.jsonBody <| -- TODO: https://developer.github.com/v3/issues/#edit-an-issue
            Encode.object
                [ ( "title", Encode.string issue.title )
                , ( "body", Encode.string issue.description )
                , ( "assignees", issue.assignees
                    |> List.map .login
                    |> List.map Encode.string
                    |> Encode.list
                    )
                , ( "labels", issue.labels
                    |> List.map .name
                    |> List.map Encode.string
                    |> Encode.list
                    )
                , ( "state", Encode.string issue.state )
                , ( "milestone", Encode.null )
                {-
                , case issue.milestone of
                    Just m ->
                        ( "milestone", m.number
                            |> String.toInt
                            |> Result.toMaybe
                            |> Maybe.withDefault 0
                            |> Encode.int
                        )
                    Nothing ->
                        ( "milestone", Encode.null )
                        -}
                ]
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send onComplete


fetchMilestones : String -> String -> Cmd Msg
fetchMilestones repo accessToken =
    Http.request
        { method = "GET"
        , headers = [ Http.header "If-Modified-Since" "0"]
        , url =
            "https://api.github.com/repos/"
                ++ repo
                ++ "/milestones?access_token="
                ++ accessToken
        , expect = Http.expectJson <| Decode.list milestoneDecoder
        , body = Http.emptyBody
        , timeout = Nothing
        , withCredentials = False
        }
            |> Http.send LoadMilestones


fetchUser : String -> Cmd Msg
fetchUser accessToken =
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
                    Decode.decodeString userDecoder res.body
                else
                    Err "Insufficient permissions: 'repo' oauth scope is required"
                )
        , body = Http.emptyBody
        , timeout = Nothing
        , withCredentials = False
        }
            |> Http.send LoadUser
