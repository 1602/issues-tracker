module Request.Issue exposing (create, list, listForMilestone, update, search)

import Http exposing (Error, Response)
import Data.Issue as Issue exposing (Issue)
import Data.Milestone as Milestone exposing (Milestone)
import Json.Decode exposing (Value)
import Models exposing (Model, Column(..), Filter(..), IssueState(..))
import Date.Extra as Date exposing (Interval(..))
import Messages exposing (Msg(..))
import Request.Helpers exposing (cachingFetch)
import Json.Encode as Encode

create : String -> Maybe String -> Value -> (Result Error Issue -> a) -> Cmd a
create repo accessToken data onComplete =
    Http.request
        { method = "POST"
        , headers = []
        , url =
            "https://api.github.com/repos/" ++ repo ++ "/issues"
            ++ "?access_token=" ++ (Maybe.withDefault "" accessToken)
        , expect = Http.expectJson <| Issue.decoder
        , body = Http.jsonBody data
        , timeout = Nothing
        , withCredentials = False
        }
            |> Http.send onComplete


list : Model -> Column -> Cmd Msg
list model column =
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
                ++ "/issues"
                ++ "?sort=updated"
                ++ labels
                ++ state
                ++ milestone
                ++ filterByUser
                ++ since
    in
        cachingFetch
            url
            accessToken
            model.etags
            (IssuesLoaded column)

listForMilestone : Model -> IssueState -> Milestone -> Cmd Msg
listForMilestone model issueState ms =
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
                ++ "/issues?"
                ++ state
                ++ "&sort=updated"
                ++ "&milestone=" ++ ms.number
                ++ filterByUser
                ++ since
    in
        cachingFetch
            url
            accessToken
            model.etags
            (MilestoneIssuesLoaded ms.number issueState)


update : String -> String -> Encode.Value -> String -> (Result Error Issue -> a) -> Cmd a
update repo issueNumber issue accessToken onComplete =
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


search : Model -> Cmd Msg
search { repo, accessToken, searchTerms, etags } =
    let
        url =
            "https://api.github.com/search/issues?"
                ++ "q=repo:" ++ repo ++ " " ++ searchTerms
    in
        cachingFetch
            url
            (accessToken |> Maybe.withDefault "")
            etags
            IssuesSearchResults
