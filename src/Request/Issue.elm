module Request.Issue exposing (create, list, listForMilestone, update, search)

import Http exposing (Error, Response)
import Data.Issue as Issue exposing (Issue)
import Data.Milestone as Milestone exposing (Milestone)
import Json.Decode exposing (Value)
import Models exposing (Model, Filter(..), IssueState(..))
import Date.Extra as Date exposing (Interval(..))
import Messages exposing (Msg(..))
import Request.Helpers exposing (withAuthorization, apiUrl)
import Request.Cache exposing (cachingFetch)
import Json.Encode as Encode
import HttpBuilder
import Data.Column exposing (Column(..))


create : (String, String) -> String -> Value -> (Result Error Issue -> a) -> Cmd a
create (user, repo) accessToken data onComplete =
    apiUrl ("/repos/" ++ user ++ "/" ++ repo ++ "/issues")
        |> HttpBuilder.post
        |> HttpBuilder.withExpect (Http.expectJson Issue.decoder)
        |> HttpBuilder.withBody (Http.jsonBody data)
        |> withAuthorization accessToken
        |> HttpBuilder.toRequest
        |> Http.send onComplete


list : Model -> Column -> Http.Request (RemoteData (List Issue))
list model column =
    let
        filter =
            model.filter

        (user, repo) =
            model.repo

        accessToken =
            model.persistentData.accessToken

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
            model.now
                |> Date.add interval duration
                |> Date.floor Hour
                |> Date.toUtcIsoString
                |> (++) "&since="

        since =
            case column of
                Done ->
                    case model.persistentData.doneLimit of
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
        expect =
            Http.expectJson (Decode.list Issue.decoder)

        url =
            apiUrl <|
                "/repos/"
                    ++ user
                    ++ "/"
                    ++ repo
                    ++ "/issues"
                    ++ "?sort=updated"
                    ++ labels
                    ++ state
                    ++ milestone
                    ++ filterByUser
                    ++ since
    in
        url
            |> withAuthorization accessToken
            |> withCache model.etags expect
            |> HttpBuilder.toRequest


listForMilestone : Model -> IssueState -> Milestone -> Cmd Msg
listForMilestone model issueState ms =
    let
        filter =
            model.filter

        (user, repo) =
            model.repo

        accessToken =
            model.persistentData.accessToken

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
            model.now
                |> Date.add interval duration
                |> Date.floor Hour
                |> Date.toUtcIsoString
                |> (++) "&since="

        since =
            case issueState of
                ClosedIssue ->
                    case model.persistentData.doneLimit of
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
        cachingFetch
            url
            accessToken
            model.etags
            (MilestoneIssuesLoaded ms.number issueState)


update : (String, String) -> String -> Encode.Value -> String -> (Result Error Issue -> a) -> Cmd a
update (user, repo) issueNumber issue accessToken onComplete =
    apiUrl ("/repos/" ++ user ++ "/" ++ repo ++ "/issues/" ++ issueNumber)
        |> HttpBuilder.patch
        |> HttpBuilder.withExpect (Http.expectJson Issue.decoder)
        |> withAuthorization accessToken
        |> HttpBuilder.withBody (Http.jsonBody issue)
        |> HttpBuilder.toRequest
        |> Http.send onComplete


search : Model -> Cmd Msg
search { repo, persistentData, searchTerms, etags } =
    let
        (u, r) = repo
    in
    cachingFetch
        (apiUrl <| "/search/issues?" ++ "q=repo:" ++ u ++ "/" ++ r ++ " " ++ searchTerms)
        persistentData.accessToken
        etags
        IssuesSearchResults
