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
import Date.Extra as Date exposing (Interval(..))
import Models exposing (Model)


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


createIssue : String -> Maybe String -> Encode.Value -> (Result Error Issue -> a) -> Cmd a
createIssue repo accessToken data onComplete =
    Http.request
        { method = "POST"
        , headers = []
        , url =
            "https://api.github.com/repos/" ++ repo ++ "/issues"
            ++ "?access_token=" ++ (Maybe.withDefault "" accessToken)
        , expect = Http.expectJson <| issueDecoder
        , body = Http.jsonBody data
        , timeout = Nothing
        , withCredentials = False
        }
            |> Http.send onComplete


fetchMilestoneIssues : Model -> IssueState -> Milestone -> Cmd Msg
fetchMilestoneIssues model issueState ms =
    let
        filter =
            model.filter

        repo =
            model.repo

        accessToken =
            Maybe.withDefault "" model.accessToken

        state =
            case issueState of
                OpenIssue ->
                    "&state=open"

                ClosedIssue ->
                    "&state=closed"

        filterByUser =
            case filter of
                CreatedBy user ->
                    "&creator=" ++ user

                AssignedTo user ->
                    "&assignee=" ++ user

                HasMentionOf user ->
                    "&mentioned=" ++ user

                All ->
                    ""

        pastMoment duration interval =
            model.now
                |> Date.add interval duration
                |> Date.floor Hour
                |> Date.toUtcIsoString
                |> (++) "&since="

        since =
            case issueState of
                ClosedIssue ->
                    case model.settings.doneLimit of
                        "a day" ->
                            pastMoment -1 Day

                        "a week" ->
                            pastMoment -1 Week

                        "two weeks" ->
                            pastMoment -2 Week

                        "a month" ->
                            pastMoment -1 Month

                        _ ->
                            ""
                OpenIssue ->
                    ""
        url =
            "https://api.github.com/repos/"
                ++ repo
                ++ "/issues?access_token="
                ++ accessToken
                ++ state
                ++ "&sort=updated"
                ++ "&milestone=" ++ ms.number
                ++ filterByUser
                ++ since
    in
        cachingFetch
            url
            model.etags
            (MilestoneIssuesLoaded ms.number issueState)

fetchIssues : Model -> Column -> Cmd Msg
fetchIssues model column =
    let
        filter =
           model.filter

        repo =
           model.repo

        accessToken =
            Maybe.withDefault "" model.accessToken

        milestone =
            case column of
                Icebox ->
                    "&milestone=none"

                Done ->
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
                    "&labels=Status: Completed"

        state =
            case column of
                Done ->
                    "&state=closed"

                _ ->
                    ""

        filterByUser =
            case filter of
                CreatedBy user ->
                    "&creator=" ++ user

                AssignedTo user ->
                    "&assignee=" ++ user

                HasMentionOf user ->
                    "&mentioned=" ++ user

                All ->
                    ""

        pastMoment duration interval =
            model.now
                |> Date.add interval duration
                |> Date.floor Hour
                |> Date.toUtcIsoString
                |> (++) "&since="

        since =
            case column of
                Done ->
                    case model.settings.doneLimit of
                        "a day" ->
                            pastMoment -1 Day

                        "a week" ->
                            pastMoment -1 Week

                        "two weeks" ->
                            pastMoment -2 Week

                        "a month" ->
                            pastMoment -1 Month

                        _ ->
                            ""
                _ ->
                    ""
        url =
            "https://api.github.com/repos/"
                ++ repo
                ++ "/issues?access_token="
                ++ accessToken
                ++ "&sort=updated"
                ++ labels
                ++ state
                ++ milestone
                ++ filterByUser
                ++ since
    in
        cachingFetch
            url
            model.etags
            (IssuesLoaded column)


cachingFetch : String -> Dict.Dict String String -> (Maybe String -> Msg) -> Cmd Msg
cachingFetch url etags oncomplete =
    Http.request
        { method = "GET"
        , headers =
            case Dict.get url etags of
                Just etag ->
                    [ Http.header "If-None-Match" etag ]

                Nothing ->
                    []
        , url = url
        , expect = Http.expectStringResponse (\res ->
            if res.status.code == 304 then
                Ok NotModified
            else
                case res.headers |> Debug.log ("headers " ++ url) |> Dict.toList |> List.filterMap (\(key, val) -> if "etag" == (String.toLower key) then Just val else Nothing) |> List.head of
                    Just etag ->
                        Ok <| CachedData url etag res.body

                    Nothing ->
                        Ok <| NotCached res.body
            )
        , body = Http.emptyBody
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send (FetchComplete oncomplete)


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


fetchMilestones : Model -> Cmd Msg
fetchMilestones model =
    let
        url =
            "https://api.github.com/repos/"
                ++ model.repo
                ++ "/milestones?access_token="
                ++ (Maybe.withDefault "" model.accessToken)
    in
        cachingFetch
            url
            model.etags
            LoadMilestones


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


checkVersion : Dict.Dict String String -> Cmd Msg
checkVersion etags =
    cachingFetch
        "https://raw.githubusercontent.com/1602/issues-tracker/master/package.json"
        etags
        CheckVersion
