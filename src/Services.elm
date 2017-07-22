module Services exposing (..)

import Json.Decode as Decode
import Base64
-- import Task
import Http exposing (Error, Response)
import Messages exposing (..)
import Models exposing (..)
import Json.Encode as Encode
import Dict
import Date.Extra as Date exposing (Interval(..))
import Models exposing (Model)
import Data.Milestone as Milestone exposing (Milestone)
import Data.Issue as Issue exposing (Issue)
import Data.User as User


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
        , expect = Http.expectJson <| Milestone.decoder
        , body = Http.jsonBody <|
            Encode.object
                [ ( "title", Encode.string title )
                ]
        , timeout = Nothing
        , withCredentials = False
        }
            |> Http.send MilestoneCreated


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



cachingFetch : String -> Dict.Dict String (String, String) -> (String -> Msg) -> Cmd Msg
cachingFetch url etags oncomplete =
    let
        (etag, cachedBody) =
            case Dict.get url etags of
                Just (etag, body) ->
                    (etag, body)

                Nothing ->
                    ("", "")

        extractEtag res =
            res.headers
                |> Dict.toList
                |> List.filterMap (\(key, val) ->
                    if "etag" == (String.toLower key) then
                        Just val
                    else
                        Nothing
                    )
                |> List.head
    in
        Http.request
            { method = "GET"
            , headers =
                if etag /= "" then
                    [ Http.header "If-None-Match" etag ]
                else
                    [ Http.header "If-Modified-Since" "0" ]
            , url = url
            , expect = Http.expectStringResponse (\res ->
                if res.status.code == 304 then
                    Ok <| CachedData res.url etag cachedBody
                else
                    case extractEtag res of
                        Just etag ->
                            Ok <| CachedData res.url etag res.body

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
        , expect = Http.expectJson Issue.decoder
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
        , expect = Http.expectJson Issue.decoder
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
                    Decode.decodeString User.decoder res.body
                else
                    Err "Insufficient permissions: 'repo' oauth scope is required"
                )
        , body = Http.emptyBody
        , timeout = Nothing
        , withCredentials = False
        }
            |> Http.send LoadUser

searchIssues : Model -> Cmd Msg
searchIssues { repo, accessToken, searchTerms, etags } =
    let
        url =
            "https://api.github.com/search/issues?access_token="
                ++ (Maybe.withDefault "" accessToken)
                ++ "&q=repo:" ++ repo ++ " " ++ searchTerms
    in
        cachingFetch
            url
            etags
            IssuesSearchResults
