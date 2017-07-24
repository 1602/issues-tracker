module Request.Issue exposing (create, list, listForMilestone, update, search)

import Http exposing (Error, Response, Request)
import Data.Issue as Issue exposing (Issue, IssueState(..))
import Data.Milestone as Milestone exposing (Milestone)
import Json.Decode as Decode exposing (Value)
import Date exposing (Date)
import Date.Extra as Date exposing (Interval(..))
import Request.Helpers exposing (withAuthorization, apiUrl)
import Request.Cache exposing (Etags, withCache, CachedResult, CachedRequest)
import Json.Encode as Encode
import HttpBuilder
import Data.Column exposing (Column(..))
import Data.Filter exposing (Filter(..))


create : (String, String) -> String -> Value -> (Result Error Issue -> a) -> Cmd a
create (user, repo) accessToken data onComplete =
    apiUrl ("/repos/" ++ user ++ "/" ++ repo ++ "/issues")
        |> HttpBuilder.post
        |> HttpBuilder.withExpect (Http.expectJson Issue.decoder)
        |> HttpBuilder.withBody (Http.jsonBody data)
        |> withAuthorization accessToken
        |> HttpBuilder.toRequest
        |> Http.send onComplete


list : String -> Date -> String -> Filter -> (String, String) -> Column -> Etags -> CachedRequest (List Issue)
list doneLimit now accessToken filter (user, repo) column etags =
    let

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

                Search ->
                    ""

                Backlog ->
                    "&labels=Status: Ready"

                Current ->
                    "&labels=Status: In Progress"

                Done ->
                    -- "&labels=Status: Completed"
                    ""

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
            now
                |> Date.add interval duration
                |> Date.floor Hour
                |> Date.toUtcIsoString
                |> (++) "&since="

        since =
            case column of
                Done ->
                    case doneLimit of
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
        decoder =
            Decode.list Issue.decoder

    in
        apiUrl ("/repos/" ++ user ++ "/" ++ repo ++ "/issues" ++ "?sort=updated" ++ labels ++ state ++ milestone ++ filterByUser ++ since)
            |> HttpBuilder.get
            |> withAuthorization accessToken
            |> withCache etags decoder
            |> HttpBuilder.toRequest


listForMilestone : String -> Date -> String -> Filter -> (String, String) -> IssueState -> Milestone -> Etags -> CachedRequest (List Issue)
listForMilestone doneLimit now accessToken filter (user, repo) issueState ms etags =
    let

        state =
            case issueState of
                OpenIssue ->
                    "state=open"

                ClosedIssue ->
                    "state=closed"

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
            now
                |> Date.add interval duration
                |> Date.floor Hour
                |> Date.toUtcIsoString
                |> (++) "&since="

        since =
            case issueState of
                ClosedIssue ->
                    case doneLimit of
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

        decoder =
            Decode.list Issue.decoder

        url =
            apiUrl <|
                "/repos/"
                    ++ user
                    ++ "/"
                    ++ repo
                    ++ "/issues?"
                    ++ state
                    ++ "&sort=updated"
                    ++ "&milestone="
                    ++ ms.number
                    ++ filterByUser
                    ++ since
    in
        url
            |> HttpBuilder.get
            |> withAuthorization accessToken
            |> withCache etags decoder
            |> HttpBuilder.toRequest


update : (String, String) -> String -> Encode.Value -> String -> Request Issue
update (user, repo) issueNumber issue accessToken =
    apiUrl ("/repos/" ++ user ++ "/" ++ repo ++ "/issues/" ++ issueNumber)
        |> HttpBuilder.patch
        |> HttpBuilder.withExpect (Http.expectJson Issue.decoder)
        |> withAuthorization accessToken
        |> HttpBuilder.withBody (Http.jsonBody issue)
        |> HttpBuilder.toRequest


search : (String, String) -> String -> String -> Etags -> CachedRequest (List Issue)
search (u, r) accessToken searchTerms etags =
    let
        decoder =
            Decode.at [ "items" ] <| Decode.list Issue.decoder

    in
        apiUrl ("/search/issues?" ++ "q=repo:" ++ u ++ "/" ++ r ++ " " ++ searchTerms)
            |> HttpBuilder.get
            |> withAuthorization accessToken
            |> withCache etags decoder
            |> HttpBuilder.toRequest

