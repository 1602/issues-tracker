module Request.Issue exposing (create, list)

import Http exposing (Error, Response)
import Data.Issue as Issue exposing (Issue)
import Json.Decode exposing (Value)
import Models exposing (Model, Column(..), Filter(..))
import Date.Extra as Date exposing (Interval(..))
import Messages exposing (Msg(..))
import Services exposing (cachingFetch)

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
