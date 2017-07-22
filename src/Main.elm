port module Main exposing (..)

import Route exposing (Route, Route(..), parseHash)
import Models exposing (..)
import Messages exposing (..)
import Dict
import List
import Html exposing (Html, span, text, img, div)
import Navigation exposing (programWithFlags, Location)
import Http exposing (Error(..), Response)
import Date
import Time
import Task
import Date.Distance as Distance
import Html.Attributes as Attrs exposing (style, class, attribute, src)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Dom
import Date.Extra
import Json.Decode as Decode
import GithubMarkdown exposing (ghMd)
import Data.Issue as Issue exposing (Issue)
import Data.User as User exposing (User)
import Data.Milestone as Milestone
import Request.Issue
import Request.User
import Request.Milestone
import Data.PersistentData exposing (PersistentData)
import Data.Column exposing (Column(..))
import Json.Decode exposing (Value)


main : Program Value Model Msg
main =
    programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


extractRepo : String -> String
extractRepo hash =
    hash
        |> String.split "/"
        |> List.drop 1
        |> List.take 2
        |> String.join "/"
        |> (\s ->
                if s == "" || s == "/" then
                    "universalbasket/engineering"
                else if hash == "#/stories" then
                    "universalbasket/engineering/stories"
                else if hash == "#/milestones" then
                    "universalbasket/engineering/milestones"
                else if String.startsWith "#/stories/" hash then
                    "universalbasket/engineering" ++ (String.dropLeft 1 hash)
                else
                    s
           )



-- MODEL


init : Value -> Location -> ( Model, Cmd Msg )
init initialData location =
    let
        page =
            parseHash location

        needFocus =
            highlightStory /= ""

        highlightStory =
            case page of
                Just (Story _ _ s) ->
                    s

                _ ->
                    ""

        persistentData =
            initialData
                |> Decode.decodeValue Data.PersistentData.decoder
                |> Result.toMaybe
                |> Maybe.withDefault Data.PersistentData.default

        model =
            Model
                -- version
                "1.1.1"
                -- user
                Nothing
                -- token
                ""
                -- repo
                (extractRepo location.hash)
                -- location
                location
                -- now
                (Date.fromTime <| Time.millisecond * (toFloat 0))
                -- error
                Nothing
                -- current issues (backlog)
                Nothing
                -- icebox issues
                Nothing
                -- closed issues
                Nothing
                -- milestones
                Dict.empty
                -- pickMilestoneForIssue
                Nothing
                -- newIssueTitle
                ""
                -- highlightStory
                highlightStory
                -- newMilestoneTitle
                ""
                -- newIssueTitle
                ""
                -- needFocus
                needFocus
                -- addIssueToColumn
                Done
                -- addIssueToMilestone
                ""
                -- filter
                All
                -- filterStoriesBy
                ""
                -- etags
                Dict.empty
                -- searchTerms
                ""
                -- searchResults
                NotRequested
                -- disSwitch
                True
                -- persistentData
                persistentData

        defaultRepo =
            if persistentData.defaultRepositoryType == "specified" then
                if persistentData.defaultRepository == "" then
                    Nothing
                else
                    Just persistentData.defaultRepository
            else
                -- last visited
                List.head persistentData.recentRepos
    in
        model
            ! ([ Task.perform CurrentDate Date.now
               , if persistentData.accessToken == "" then
                    Cmd.none
                 else
                    Request.User.get persistentData.accessToken
               , case page of
                    Nothing ->
                        Navigation.modifyUrl <| "#/" ++ (Maybe.withDefault model.repo defaultRepo) ++ "/stories"

                    Just _ ->
                        Cmd.none
               ]
                ++ (loadResource model)
              )


hasLabel : String -> Issue -> Bool
hasLabel label issue =
    issue.labels
        |> List.map .name
        |> List.member label


hasNoLabel : String -> Issue -> Bool
hasNoLabel label issue =
    not <| hasLabel label issue


loadAllIssues : Model -> List (Cmd Msg)
loadAllIssues model =
    if model.didSwitch then
        [ Request.Issue.list model Current
        , Request.Issue.list model Icebox
        , Request.Issue.list model Done
        , Request.Milestone.list model
        ]
    else
        []


loadResource : Model -> List (Cmd Msg)
loadResource model =
    case model.user of
        Just user ->
            case parseHash model.location of
                Just (Settings user repo) ->
                    []

                Just (Story user repo id) ->
                    loadAllIssues model

                Just (IssuesIndex user repo) ->
                    loadAllIssues model

                Just (MilestonesIndex user repo) ->
                    loadAllIssues model

                Nothing ->
                    loadAllIssues model

        Nothing ->
            []


port navigateToIssue : (( String, String ) -> msg) -> Sub msg


port googleAuth : (String -> msg) -> Sub msg


port saveData : Value -> Cmd msg


port clipboard : String -> Cmd msg


updateLocalStorage : Model -> Cmd msg
updateLocalStorage model =
    model.persistentData
        |> Data.PersistentData.encode
        |> saveData



-- UPDATE


setFocus : String -> Cmd Msg
setFocus id =
    id
        |> Dom.focus
        |> Task.attempt
            (\s ->
                case s of
                    Ok _ ->
                        NoOp

                    Err _ ->
                        NoOp
            )


focus : Location -> Cmd Msg
focus loc =
    case parseHash loc of
        Just (Story user repo n) ->
            "story-"
                ++ n
                |> Dom.focus
                |> Task.attempt
                    (\s ->
                        case s of
                            Ok _ ->
                                StoryFocused

                            Err _ ->
                                NoOp
                    )

        _ ->
            Cmd.none


updateSettings : SettingsMsg -> PersistentData -> PersistentData
updateSettings msg settings =
    case msg of
        IgnoreIdeas ->
            { settings | powerOfNow = not settings.powerOfNow }

        ChangeDoneLimit s ->
            { settings | doneLimit = s }

        UpdateDefaultRepository s ->
            { settings | defaultRepository = s }

        ChangeDefaultRepositoryType s ->
            { settings | defaultRepositoryType = s }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- TODO: separate actions which require user
    case msg of
        NoOp ->
            model ! []

        ClearSearch ->
            { model | searchTerms = "", searchResults = NotRequested } ! []

        ToggleSaveSearch ->
            let
                pd =
                    model.persistentData

                updatedSavedSearches =
                    if model.searchTerms == "" then
                        pd.savedSearches
                    else if Dict.member model.searchTerms pd.savedSearches then
                        Dict.remove model.searchTerms pd.savedSearches
                    else
                        Dict.insert model.searchTerms model.searchTerms pd.savedSearches

                updatedPersistentData =
                    { pd | savedSearches = updatedSavedSearches }

                updatedModel =
                    { model | persistentData = updatedPersistentData }
            in
                updatedModel ! [ updateLocalStorage updatedModel ]

        IssuesSearchResults issuesJson ->
            let
                issues =
                    Decode.decodeString (Decode.at [ "items" ] <| Decode.list Issue.decoder) issuesJson

                updatedModel =
                    case issues of
                        Ok issues ->
                            { model | searchResults = Loaded issues }

                        Err e ->
                            { model | error = Just (toString e) }
            in
                updatedModel ! []

        ChangeSearchTerms terms ->
            { model | searchTerms = terms } ! []

        SearchBy s ->
            let
                updatedModel =
                    { model | searchTerms = s }
            in
                updatedModel ! [ Request.Issue.search updatedModel ]

        SearchIssues ->
            model
                ! [ Request.Issue.search model ]

        NavigateToIssue ( repo, issueNumber ) ->
            model
                ! [ Navigation.modifyUrl <| "#/" ++ repo ++ "/stories/" ++ issueNumber ]

        SettingsMsgProxy msg ->
            let
                updatedModel =
                    { model | persistentData = updateSettings msg model.persistentData }
            in
                updatedModel ! [ updateLocalStorage updatedModel ]

        FetchComplete msg result ->
            case result of
                Ok cachedData ->
                    case cachedData of
                        CachedData url etag res ->
                            update (msg res)
                                { model
                                    | etags = Dict.insert url ( etag, res ) model.etags
                                }

                        NotCached res ->
                            update (msg res) model

                Err e ->
                    case e of
                        BadStatus res ->
                            let
                                ( etag, body ) =
                                    case Dict.get res.url model.etags of
                                        Just ( etag, body ) ->
                                            ( etag, body )

                                        Nothing ->
                                            ( "", "" )
                            in
                                if res.status.code == 304 && body /= "" then
                                    update (msg body) model
                                else
                                    { model | error = toString e |> Just } ! []

                        _ ->
                            { model | error = toString e |> Just } ! []

        FilterStories s ->
            { model | filterStoriesBy = s } ! []

        PinMilestone s ->
            let
                pinnedMilestone =
                    model.persistentData.pinnedMilestones
                        |> Dict.get model.repo
                        |> Maybe.withDefault ""

                n =
                    if pinnedMilestone == s then
                        ""
                    else
                        s

                pd =
                    model.persistentData

                updatedPersistentData =
                    { pd
                        | pinnedMilestones =
                            pd.pinnedMilestones
                                |> Dict.insert model.repo n
                    }

                updatedModel =
                    { model | persistentData = updatedPersistentData }
            in
                updatedModel
                    ! (if n /= "" then
                        [ setFocus <| "milestone-" ++ n, updateLocalStorage updatedModel ]
                       else
                        [ updateLocalStorage updatedModel ]
                      )

        HideColumn s ->
            let
                pd =
                    model.persistentData

                updatedPersistentData =
                    { pd
                        | columns =
                            pd.columns
                                |> List.filter ((/=) s)
                    }

                updatedModel =
                    { model | persistentData = updatedPersistentData }
            in
                updatedModel ! [ updateLocalStorage updatedModel ]

        ReopenColumn s ->
            let
                pd =
                    model.persistentData

                updatedPersistentData =

                    { pd
                        | columns =
                            pd.columns
                                |> List.filter ((/=) s)
                                |> (::) s
                    }

                updatedModel =
                    { model | persistentData = updatedPersistentData }
            in
                updatedModel ! [ updateLocalStorage updatedModel ]

        ChangeFilter s ->
            case model.user of
                Just u ->
                    let
                        updatedModel =
                            { model
                                | filter =
                                    (case s of
                                        "assigned to me" ->
                                            AssignedTo u.login

                                        "created by me" ->
                                            CreatedBy u.login

                                        "mentioning me" ->
                                            HasMentionOf u.login

                                        _ ->
                                            All
                                    )
                            }
                    in
                        updatedModel ! loadAllIssues updatedModel

                Nothing ->
                    model ! []

        ShowIssueCreationForm col msn ->
            { model | addIssueToColumn = col, addIssueToMilestone = msn } ! [ setFocus "create-story" ]

        EditNewStoryTitle s ->
            { model | newIssueTitle = s } ! []

        CreateStory col ->
            let
                encodedIssue =
                    Encode.object
                        [ ( "title", Encode.string model.newIssueTitle )
                        , ( "body", Encode.string "" )
                        , ( "labels"
                          , Encode.list <|
                                (case col of
                                    Backlog ->
                                        case model.addIssueToMilestone of
                                            "" ->
                                                [ Encode.string "Status: Ready" ]

                                            _ ->
                                                []

                                    Current ->
                                        [ Encode.string "Status: In Progress" ]

                                    _ ->
                                        []
                                )
                          )
                        , ( "milestone"
                          , case model.addIssueToMilestone of
                                "" ->
                                    Encode.null

                                _ ->
                                    model.addIssueToMilestone
                                        |> String.toInt
                                        |> Result.withDefault 0
                                        |> Encode.int
                          )
                        , ( "assignees"
                          , case col of
                                Current ->
                                    case model.user of
                                        Just user ->
                                            Encode.list [ Encode.string user.login ]

                                        Nothing ->
                                            Encode.list []

                                _ ->
                                    Encode.list []
                          )
                        ]

                milestone =
                    model.milestones
                        |> Dict.get model.repo
                        |> Maybe.withDefault Dict.empty
                        |> Dict.get model.addIssueToMilestone
                        |> Maybe.andThen (\ms -> Just ms.milestone)

                cmd =
                    if model.newIssueTitle /= "" then
                        [ setFocus "create-story"
                        , StoryCreated col milestone
                            |> Request.Issue.create model.repo model.persistentData.accessToken encodedIssue
                        ]
                    else
                        []
            in
                { model | newIssueTitle = "", etags = Dict.empty } ! cmd

        StoryCreated col milestone result ->
            case result of
                Err error ->
                    { model | error = toString error |> Just } ! []

                Ok _ ->
                    model
                        ! [ case col of
                                Backlog ->
                                    case milestone of
                                        Just ms ->
                                            Request.Issue.listForMilestone model OpenIssue ms

                                        Nothing ->
                                            Request.Issue.list model Icebox

                                _ ->
                                    Request.Issue.list model col
                          ]

        UrgentIssueAdded result ->
            model ! [ Request.Issue.list model Icebox ]

        StoryFocused ->
            { model | needFocus = False } ! []

        EditNewMilestoneTitle s ->
            { model | newMilestoneTitle = s } ! []

        CreateNewMilestone ->
            { model | newMilestoneTitle = "" }
                ! [ Request.Milestone.create model.repo model.newMilestoneTitle model.persistentData.accessToken
                  ]

        EditAccessToken s ->
            { model | token = s } ! []

        LoadUser result ->
            case result of
                Ok user ->
                    let
                        updatedModel =
                            { model | user = Just user, error = Nothing }
                    in
                        updatedModel ! (loadResource updatedModel)

                Err e ->
                    let pd = model.persistentData
                    in
                    case e of
                        NetworkError ->
                            { model | error = Just (toString e) } ! []

                        _ ->
                            { model | error = Just (toString e), persistentData = { pd | accessToken = "" }, user = Nothing } ! []

        SaveAccessToken ->
            let
                pd = model.persistentData
                upd =
                    { pd | accessToken = model.token }
                updatedModel = { model | persistentData = upd }
            in
                if model.token == "" then
                    model ! [ Navigation.load "https://github.com/settings/tokens" ]
                else
                    updatedModel ! ([ Request.User.get model.token, updateLocalStorage updatedModel ] ++ (loadResource updatedModel))

        CurrentDate now ->
            { model | now = now } ! []

        CurrentTime now ->
            { model | now = Date.fromTime now } ! loadAllIssues model

        SelectStory issue ->
            case parseHash model.location of
                Just (Story user repo n) ->
                    model
                        ! [ if issue.number == n then
                                Navigation.modifyUrl <| "#/" ++ model.repo ++ "/stories"
                            else
                                Navigation.modifyUrl <| "#/" ++ model.repo ++ "/stories/" ++ issue.number
                          ]

                Just (IssuesIndex user repo) ->
                    model ! [ Navigation.modifyUrl <| "#/" ++ model.repo ++ "/stories/" ++ issue.number ]

                _ ->
                    model ! []

        UrlChange location ->
            let
                newRepository =
                    extractRepo location.hash

                didSwitch =
                    newRepository /= model.repo

                prependNewRepositoryToRecent =
                    newRepository
                        :: (model.persistentData.recentRepos
                                |> List.filter ((/=) newRepository)
                                |> List.take 19
                           )

                highlightStory =
                    case parseHash location of
                        Just (Story _ _ s) ->
                            s

                        _ ->
                            ""

                needFocus =
                    (highlightStory /= "") && (highlightStory /= model.highlightStory)

                recentRepos =
                    if didSwitch then
                        model.persistentData.recentRepos
                    else
                        prependNewRepositoryToRecent

                pd =
                    model.persistentData

                updatedModel =
                    { model
                        | location = location
                        , didSwitch = didSwitch
                        , repo = newRepository
                        , needFocus = Debug.log "need focus" needFocus
                        , highlightStory = highlightStory
                        , persistentData = { pd | recentRepos = recentRepos }
                    }
            in
                updatedModel
                    ! (loadResource updatedModel
                        ++ (if needFocus then
                                [ focus <| Debug.log "loc" location ]
                            else
                                []
                           )
                      )

        MilestoneIssuesLoaded num issueState issuesJson ->
            let
                issues json =
                    Decode.decodeString (Decode.list Issue.decoder) json
                        |> Result.toMaybe
                        |> Maybe.withDefault []

                updateMilestoneIssues issues s =
                    case s of
                        Just ms ->
                            case issueState of
                                OpenIssue ->
                                    let
                                        m =
                                            ms.milestone

                                        updatedMilestone =
                                            { m | openIssues = List.length issues }
                                    in
                                        Just
                                            { ms
                                                | openIssues = Just issues
                                                , milestone = updatedMilestone
                                            }

                                ClosedIssue ->
                                    let
                                        m =
                                            ms.milestone

                                        updatedMilestone =
                                            { m | closedIssues = List.length issues }
                                    in
                                        Just
                                            { ms
                                                | closedIssues = Just issues
                                                , milestone = updatedMilestone
                                            }

                        Nothing ->
                            Nothing

                updatedMilestones =
                    Dict.update model.repo
                        (\milestones ->
                            Just
                                (Maybe.withDefault Dict.empty milestones
                                    |> Dict.update num (updateMilestoneIssues <| issues issuesJson)
                                )
                        )
                        model.milestones

                updatedModel =
                    { model
                        | milestones = updatedMilestones
                    }

                mss =
                    case Dict.get model.repo updatedModel.milestones of
                        Just milestones ->
                            Dict.values milestones

                        Nothing ->
                            []

                loaded =
                    List.foldl
                        (\ms res ->
                            case issueState of
                                ClosedIssue ->
                                    case ms.closedIssues of
                                        Nothing ->
                                            if ms.milestone.closedIssues > 0 then
                                                res
                                            else
                                                res + 1

                                        Just _ ->
                                            res + 1

                                OpenIssue ->
                                    case ms.openIssues of
                                        Nothing ->
                                            if ms.milestone.openIssues > 0 then
                                                res
                                            else
                                                res + 1

                                        Just _ ->
                                            res + 1
                        )
                        0
                        mss

                isFullyLoaded =
                    (List.length mss) == loaded
            in
                updatedModel
                    ! (if isFullyLoaded && model.needFocus then
                        [ focus model.location ]
                       else
                        []
                      )

        LoadMilestones milestonesJson ->
            let
                result =
                    milestonesJson
                        |> Decode.decodeString (Decode.list Milestone.decoder)
            in
                case result of
                    Err error ->
                        { model | error = Just (toString error) } ! []

                    Ok milestones ->
                        let
                            recentRepos =
                                if (Maybe.withDefault "" <| List.head model.persistentData.recentRepos) == model.repo then
                                    model.persistentData.recentRepos
                                else
                                    model.repo
                                        :: (model.persistentData.recentRepos
                                                |> List.filter ((/=) model.repo)
                                                |> List.take 19
                                           )

                            updatedMilestones =
                                milestones
                                    |> List.filter
                                        (\ms ->
                                            not model.persistentData.powerOfNow || (ms.dueOn /= Nothing)
                                        )
                                    |> List.foldl
                                        (\ms ->
                                            Dict.update ms.number
                                                (\m ->
                                                    Just <|
                                                        case m of
                                                            Just m ->
                                                                { m | milestone = ms }

                                                            Nothing ->
                                                                ExpandedMilestone
                                                                    ms
                                                                    (if ms.openIssues > 0 then
                                                                        Nothing
                                                                     else
                                                                        Just []
                                                                    )
                                                                    (if ms.closedIssues > 0 then
                                                                        Nothing
                                                                     else
                                                                        Just []
                                                                    )
                                                )
                                        )
                                        (Dict.get model.repo model.milestones |> Maybe.withDefault Dict.empty)

                            pd = model.persistentData

                            updatedModel =
                                { model
                                    | milestones = Dict.insert model.repo updatedMilestones model.milestones
                                    , persistentData = { pd | recentRepos = recentRepos }
                                    , error = Nothing
                                }
                        in
                            updatedModel
                                ! (if model.persistentData.accessToken /= "" then
                                        (updateLocalStorage updatedModel)
                                            :: (updatedMilestones
                                                    |> Dict.values
                                                    |> List.map .milestone
                                                    |> List.filter (\ms -> ms.openIssues > 0)
                                                    |> List.map (Request.Issue.listForMilestone model OpenIssue)
                                               )
                                            ++ (updatedMilestones
                                                    |> Dict.values
                                                    |> List.map .milestone
                                                    |> List.filter (\ms -> ms.closedIssues > 0)
                                                    |> List.map (Request.Issue.listForMilestone model ClosedIssue)
                                               )

                                    else
                                        []
                                  )

        MilestoneCreated result ->
            case result of
                Ok _ ->
                    let
                        s =
                            model.persistentData
                    in
                        { model
                            | error =
                                Nothing
                                -- TODO get rid of hack by applying powerOfNow setting on a view level (and separating list on issues with the list of milestones)
                            , persistentData = { s | powerOfNow = False }
                        }
                            ! [ Request.Milestone.list model
                              ]

                Err e ->
                    { model | error = toString e |> Just } ! []

        IssuesLoaded column issuesJson ->
            let
                issues json =
                    Decode.decodeString (Decode.list Issue.decoder) json
                        |> Result.toMaybe
            in
                case column of
                    Current ->
                        { model | currentIssues = issues issuesJson, error = Nothing }
                            ! (if model.needFocus then
                                [ focus model.location ]
                               else
                                []
                              )

                    Icebox ->
                        { model | iceboxIssues = issues issuesJson, error = Nothing }
                            ! (if model.needFocus then
                                [ focus model.location ]
                               else
                                []
                              )

                    Done ->
                        { model | closedIssues = issues issuesJson, error = Nothing }
                            ! (if model.needFocus then
                                [ focus model.location ]
                               else
                                []
                              )

                    _ ->
                        model ! []

        CopyText str ->
            model ! [ clipboard str ]

        UnsetMilestone m result ->
            { model | lockedIssueNumber = "", etags = Dict.empty }
                ! [ Request.Issue.listForMilestone model OpenIssue m
                  , Request.Issue.list model Icebox
                  ]

        SetMilestone issue milestone ->
            if model.persistentData.accessToken /= "" then
                { model
                    | pickMilestoneForIssue = Nothing
                    , lockedIssueNumber = issue.number
                }
                    ! [ Request.Issue.update model.repo
                            issue.number
                            (Encode.object
                                [ ( "milestone"
                                  , milestone.number
                                        |> String.toInt
                                        |> Result.toMaybe
                                        |> Maybe.withDefault 0
                                        |> Encode.int
                                  )
                                ]
                            )
                            model.persistentData.accessToken
                            (MilestoneSet milestone)
                      ]

            else
                model ! []

        MilestoneSet m result ->
            { model | lockedIssueNumber = "", etags = Dict.empty }
                ! [ Request.Issue.listForMilestone model OpenIssue m
                  , Request.Issue.list model Icebox
                  ]

        IssueRestarted m result ->
            { model | lockedIssueNumber = "", etags = Dict.empty }
                ! case m of
                    Just milestone ->
                        [ Request.Issue.listForMilestone model ClosedIssue milestone
                        , Request.Issue.list model Current
                        ]

                    Nothing ->
                        [ Request.Issue.list model Done
                        , Request.Issue.list model Current
                        ]

        IssueStarted milestone result ->
            { model | lockedIssueNumber = "", etags = Dict.empty }
                ! case milestone of
                    Just m ->
                        [ Request.Issue.listForMilestone model OpenIssue m
                        , Request.Issue.list model Current
                        ]

                    Nothing ->
                        [ Request.Issue.list model Current
                        , Request.Issue.list model Icebox
                        ]

        IssueFinished m result ->
            { model | lockedIssueNumber = "", etags = Dict.empty }
                ! case m of
                    Just m ->
                        [ Request.Issue.listForMilestone model ClosedIssue m
                        , Request.Issue.list model Current
                        ]

                    Nothing ->
                        [ Request.Issue.list model Current
                        , Request.Issue.list model Done
                        ]

        DismissPlanningIssue ->
            { model | pickMilestoneForIssue = Nothing } ! []

        IssueAction issue action ->
            if model.persistentData.accessToken /= "" then
                    case action of
                        "unplan" ->
                            case issue.milestone of
                                Just m ->
                                    { model | lockedIssueNumber = issue.number }
                                        ! [ UnsetMilestone m
                                                |> Request.Issue.update model.repo
                                                    issue.number
                                                    (Encode.object [ ( "milestone", Encode.null ) ])
                                                    model.persistentData.accessToken
                                          ]

                                Nothing ->
                                    model ! []

                        "start" ->
                            case model.user of
                                Just user ->
                                    { model | lockedIssueNumber = issue.number }
                                        ! [ Request.Issue.update model.repo
                                                issue.number
                                                (Encode.object
                                                    [ ( "labels"
                                                      , issue.labels
                                                            |> List.map .name
                                                            |> List.filter ((/=) "Status: In Progress")
                                                            |> List.filter ((/=) "Status: Ready")
                                                            |> (::) "Status: In Progress"
                                                            |> List.map Encode.string
                                                            |> Encode.list
                                                      )
                                                    , ( "assignees"
                                                      , [ Encode.string user.login ] |> Encode.list
                                                      )
                                                    ]
                                                )
                                                model.persistentData.accessToken
                                                (IssueStarted issue.milestone)
                                          ]

                                Nothing ->
                                    model ! []

                        "finish" ->
                            { model | lockedIssueNumber = issue.number }
                                ! [ Request.Issue.update model.repo
                                        issue.number
                                        (Encode.object
                                            [ ( "labels"
                                              , issue.labels
                                                    |> List.map .name
                                                    |> List.filter ((/=) "Status: In Progress")
                                                    |> (::) "Status: Completed"
                                                    |> List.map Encode.string
                                                    |> Encode.list
                                              )
                                            , ( "state", Encode.string "closed" )
                                            ]
                                        )
                                        model.persistentData.accessToken
                                        (IssueFinished issue.milestone)
                                  ]

                        "restart" ->
                            { model | lockedIssueNumber = issue.number }
                                ! [ Request.Issue.update model.repo
                                        issue.number
                                        (Encode.object
                                            [ ( "labels"
                                              , issue.labels
                                                    |> List.map .name
                                                    |> List.filter ((/=) "Status: Completed")
                                                    |> List.filter ((/=) "Status: In Progress")
                                                    |> (::) "Status: In Progress"
                                                    |> List.map Encode.string
                                                    |> Encode.list
                                              )
                                            , ( "state", Encode.string "open" )
                                            ]
                                        )
                                        model.persistentData.accessToken
                                        (IssueRestarted issue.milestone)
                                  ]

                        "unstart" ->
                            { model | lockedIssueNumber = issue.number }
                                ! [ Request.Issue.update model.repo
                                        issue.number
                                        (Encode.object
                                            [ ( "labels"
                                              , issue.labels
                                                    |> List.map .name
                                                    |> List.filter ((/=) "Status: Ready")
                                                    |> List.filter ((/=) "Status: In Progress")
                                                    |> (\labels ->
                                                            case issue.milestone of
                                                                Nothing ->
                                                                    "Status: Ready" :: labels

                                                                Just _ ->
                                                                    labels
                                                       )
                                                    |> List.map Encode.string
                                                    |> Encode.list
                                              )
                                            ]
                                        )
                                        model.persistentData.accessToken
                                        (IssueStarted issue.milestone)
                                  ]

                        "plan" ->
                            { model | pickMilestoneForIssue = Just issue } ! []

                        "ice" ->
                            model
                                ! [ Request.Issue.update model.repo
                                        issue.number
                                        (Encode.object
                                            [ ( "labels"
                                              , issue.labels
                                                    |> List.map .name
                                                    |> List.filter ((/=) "Status: Ready")
                                                    |> List.map Encode.string
                                                    |> Encode.list
                                              )
                                            ]
                                        )
                                        model.persistentData.accessToken
                                        UrgentIssueAdded
                                  ]

                        "just do it" ->
                            model
                                ! [ Request.Issue.update model.repo
                                        issue.number
                                        (Encode.object
                                            [ ( "labels"
                                              , issue.labels
                                                    |> List.map .name
                                                    |> (::) "Status: Ready"
                                                    |> List.map Encode.string
                                                    |> Encode.list
                                              )
                                            ]
                                        )
                                        model.persistentData.accessToken
                                        UrgentIssueAdded
                                  ]

                        _ ->
                            model ! []

                else
                    model ! []


save : Result Error a -> Model -> (a -> Model) -> ( Model, Cmd Msg )
save result model fn =
    case result of
        Ok x ->
            let
                m =
                    fn x
            in
                { m | error = Nothing } ! []

        Err error ->
            { model | error = toString error |> Just } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (30 * Time.second) CurrentTime
        , navigateToIssue NavigateToIssue
        ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        error =
            case model.error of
                Just error ->
                    div
                        [ style
                            [ ( "position", "fixed" )
                            , ( "top", "10px" )
                            , ( "right", "10px" )
                            , ( "border", "1px solid red" )
                            , ( "color", "crimson" )
                            , ( "background", "#111" )
                            , ( "padding", "10px" )
                            , ( "border-radius", "2px" )
                            , ( "max-width", "500px" )
                            , ( "max-height", "350px" )
                            , ( "overflow", "auto" )
                            , ( "box-shadow", "0px 0px 17px 9px rgba(5,5,5,0.35)" )
                            ]
                        ]
                        [ text error ]

                Nothing ->
                    text ""

        pickMilestoneModal issue =
            div
                [ style
                    [ ( "position", "fixed" )
                    , ( "top", "70px" )
                    , ( "left", "50%" )
                    , ( "margin-left", "-200px" )
                    , ( "width", "400px" )
                    , ( "padding", "10px" )
                    , ( "background", "#777" )
                    , ( "border", "2px solid #bbb" )
                    ]
                ]
                [ div [] [ text "Select milestone for issue " ]
                , Html.strong [] [ text <| "#" ++ issue.number ]
                , text <| " " ++ issue.title
                , Html.hr [] []
                , model.milestones
                    |> Dict.get model.repo
                    |> Maybe.withDefault Dict.empty
                    |> Dict.values
                    |> List.map
                        (\s ->
                            Html.li [ style [ ( "list-style", "none" ) ] ] [ Html.button [ onClick <| SetMilestone issue s.milestone ] [ text s.milestone.title ] ]
                        )
                    |> (\list ->
                            list
                                ++ [ Html.li [ style [ ( "list-style", "none" ) ] ]
                                        [ Html.input [ onInput EditNewMilestoneTitle ] []
                                        , Html.button [ onClick CreateNewMilestone ] [ text "Create" ]
                                        ]
                                   ]
                       )
                    |> Html.ul []
                , Html.hr [] []
                , Html.button [ onClick DismissPlanningIssue ] [ text "Dismiss" ]
                ]
    in
        div [ style [ ( "display", "flex" ) ] ] <|
            [ viewNavigation model.user model
            , if model.persistentData.accessToken == "" then
                viewPage model.user model <| parseHash model.location
              else
                div []
                    [ text "cheers. visit "
                    , Html.a [ Attrs.href "https://github.com/settings/tokens" ] [ text "https://github.com/settings/tokens" ]
                    , text " (we need 'repo' access granted to see all private repositories)"
                    , Html.br [] []
                    , text "and fill this input "
                    , Html.input [ onInput EditAccessToken ] []
                    , Html.button [ onClick SaveAccessToken ] [ text "then press this button" ]
                    ]
            , error
            , model.pickMilestoneForIssue
                |> Maybe.andThen (pickMilestoneModal >> Just)
                |> Maybe.withDefault (text "")
            ]


viewPage : Maybe User -> Model -> Maybe Route -> Html Msg
viewPage user model route =
    let
        displayIssuesWithinMilestones milestones issueState =
            case milestones of
                Just milestones ->
                    let
                        mss =
                            Dict.values milestones

                        loaded =
                            List.foldl
                                (\ms res ->
                                    case issueState of
                                        ClosedIssue ->
                                            case ms.closedIssues of
                                                Nothing ->
                                                    if ms.milestone.closedIssues > 0 then
                                                        res
                                                    else
                                                        res + 1

                                                Just _ ->
                                                    res + 1

                                        OpenIssue ->
                                            case ms.openIssues of
                                                Nothing ->
                                                    if ms.milestone.openIssues > 0 then
                                                        res
                                                    else
                                                        res + 1

                                                Just _ ->
                                                    res + 1
                                )
                                0
                                mss

                        total =
                            List.length mss
                    in
                        if total == loaded then
                            Just <| listIssuesWithinMilestones milestones issueState model.now model
                        else
                            Just <| span [ cellStyle "calc(100% - 8px)" ] [ text <| "Loading milestones (" ++ (toString loaded) ++ " of " ++ (toString total) ++ ")..." ]

                Nothing ->
                    Nothing

        displayIssues head filter issues col addto milestoneNumber =
            case issues of
                Just issues ->
                    listIssues head True (List.filter filter issues) col model addto milestoneNumber

                Nothing ->
                    span [ cellStyle "calc(100% - 8px)" ] [ text "Loading..." ]

        displayIssuesGroupedByDate issues col =
            let
                daysSince date =
                    Date.Extra.diff Date.Extra.Day date model.now

                groups =
                    issues
                        |> List.foldl
                            (\a res ->
                                if daysSince a.updatedAt <= 1 then
                                    { res | today = res.today ++ [ a ] }
                                else if daysSince a.updatedAt <= 2 then
                                    { res | yesterday = res.yesterday ++ [ a ] }
                                else if daysSince a.updatedAt <= 7 then
                                    { res | week = res.week ++ [ a ] }
                                else
                                    { res | earlier = res.earlier ++ [ a ] }
                            )
                            { today = []
                            , yesterday = []
                            , week = []
                            , earlier = []
                            }

                append list title add result =
                    if List.isEmpty list && not add then
                        result
                    else if List.isEmpty list && add then
                        result ++ [ listIssues ( Nothing, text title ) add list col model col "" ]
                    else
                        result ++ [ listIssues ( Nothing, text title ) add list col model col "" ]
            in
                []
                    |> append groups.today "Updated within a day" True
                    |> append groups.yesterday "Updated within two days" False
                    |> append groups.week "Updated within a week" False
                    |> append groups.earlier "Updated more than a week ago" False

        milestonesIndex =
            case Dict.get model.repo model.milestones of
                Just milestones ->
                    milestones
                        |> Dict.values
                        |> List.sortBy
                            (\s ->
                                case s.milestone.dueOn of
                                    Just date ->
                                        Date.toTime date |> Time.inHours

                                    Nothing ->
                                        1 / 0
                            )
                        |> List.map
                            (\s ->
                                let
                                    isOverdue =
                                        case s.milestone.dueOn of
                                            Just date ->
                                                (Date.toTime date) < (Date.toTime model.now)

                                            Nothing ->
                                                False
                                in
                                    Html.li
                                        [ style
                                            [ ( "list-style", "none" )
                                            , ( "padding", "5px" )
                                            , ( "margin", "2px" )
                                            , ( "border-bottom", "1px solid #333" )
                                            , ( "border-left"
                                              , case s.milestone.dueOn of
                                                    Just date ->
                                                        (toString
                                                            (((Date.toTime date |> Time.inHours) / 12) - ((Date.toTime model.now |> Time.inHours) / 12))
                                                        )
                                                            ++ "px solid #444"

                                                    Nothing ->
                                                        "0px"
                                              )
                                            ]
                                        ]
                                        [ Html.a [ Attrs.target "_blank", Attrs.href s.milestone.htmlUrl ] [ text <| s.milestone.title ++ " " ]
                                        , Html.span
                                            [ style
                                                [ ( "color"
                                                  , if isOverdue then
                                                        "red"
                                                    else
                                                        "grey"
                                                  )
                                                ]
                                            ]
                                            [ text <|
                                                case s.milestone.dueOn of
                                                    Just date ->
                                                        if isOverdue then
                                                            " (" ++ (Distance.inWords date model.now) ++ " overdue)"
                                                        else
                                                            " (due in " ++ (Distance.inWords date model.now) ++ ")"

                                                    Nothing ->
                                                        " (no due date)"
                                            ]
                                        ]
                            )
                        |> Html.ul [ style [ ( "zoom", "150%" ) ] ]

                Nothing ->
                    text "Loading..."

        column col colHtml content =
            let
                ( icon, title, comment ) =
                    columnTitle col
            in
                if List.member col model.persistentData.columns then
                    Html.section
                        [ style
                            [ ( "width"
                              , case List.length model.persistentData.columns of
                                    3 ->
                                        "calc(33.33% - 5px)"

                                    2 ->
                                        "calc(50% - 5px)"

                                    1 ->
                                        "calc(100% - 5px)"

                                    _ ->
                                        "calc(25% - 5px)"
                              )
                            , ( "transition", "width .1s" )
                            , ( "padding-right", "0px" )
                            , ( "padding-left", "3px" )
                            , ( "flex-shrink", "0" )
                            ]
                        ]
                        [ Html.h3 [ style [ ( "position", "relative" ) ] ]
                            [ text <| icon ++ " " ++ title ++ " "
                            , Html.small [] [ text comment ]
                            , case colHtml of
                                Just html ->
                                    html

                                Nothing ->
                                    text ""
                            , span
                                [ style
                                    [ ( "position", "absolute" )
                                    , ( "right", "1px" )
                                    , ( "top", "10px" )
                                    , ( "width", "20px" )
                                    , ( "height", "20px" )
                                    , ( "background", "#eee" )
                                    , ( "color", "#222" )
                                    , ( "line-height", "20px" )
                                    , ( "text-align", "center" )
                                    , ( "cursor", "pointer" )
                                    ]
                                , onClick <| HideColumn col
                                ]
                                [ text "×" ]
                            ]
                        , div
                            [ style
                                [ ( "overflow-y", "auto" )
                                , ( "height", "calc(100% - 43px)" )
                                ]
                            ]
                            [ Maybe.withDefault (span [ cellStyle "calc(100% - 8px)" ] [ text "Loading..." ]) content ]
                        ]
                else
                    text ""

        issuesIndex =
            Html.main_
                [ style
                    [ ( "display", "flex" )
                    , ( "height", "100vh" )
                    , ( "width", "calc(100% - 42px)" )
                    , ( "overflow-x", "auto" )
                    , ( "overflow-y", "hidden" )
                    ]
                ]
                [ column Search
                    (Just <|
                        Html.form [ class "search-issue", style [ ( "display", "inline-block" ), ( "width", "calc(100% - 128px)" ) ], Html.Events.onSubmit SearchIssues ]
                            [ Html.input [ class "search-term", Attrs.value model.searchTerms, Html.Events.onInput ChangeSearchTerms ] []
                            , case model.searchResults of
                                Loaded issues ->
                                    Html.span [ class "clear-search", onClick ClearSearch ] [ text "×" ]

                                _ ->
                                    text ""
                            , if model.searchTerms /= "" then
                                case model.searchResults of
                                    Loaded issues ->
                                        Html.span
                                            [ class
                                                (if Dict.member model.searchTerms model.persistentData.savedSearches then
                                                    "saved-search"
                                                 else
                                                    "not-saved-search"
                                                )
                                            , onClick ToggleSaveSearch
                                            ]
                                            [ text "⭐" ]

                                    _ ->
                                        text ""
                              else
                                text ""
                            ]
                    )
                    (Just <|
                        case model.searchResults of
                            NotRequested ->
                                div []
                                    [ Html.a
                                        [ cellStyle "calc(100% - 10px)"
                                        , Attrs.target "_blank"
                                        , Attrs.href "https://help.github.com/articles/searching-issues/#search-within-a-users-or-organizations-repositories"
                                        ]
                                        [ text "Github search documentation" ]
                                    , Html.ul [] <|
                                        (model.persistentData.savedSearches
                                            |> Dict.values
                                            |> List.map
                                                (\search ->
                                                    Html.li [ onClick <| SearchBy search ] [ text search ]
                                                )
                                        )
                                    ]

                            Loading ->
                                text "Loading..."

                            Loaded issues ->
                                displayIssuesGroupedByDate
                                    issues
                                    Icebox
                                    |> div []
                    )
                , column Icebox
                    Nothing
                    (model.iceboxIssues
                        |> Maybe.andThen
                            (\issues ->
                                displayIssuesGroupedByDate
                                    (List.filter (\s -> (hasNoLabel "Status: Ready" s) && (hasNoLabel "Status: In Progress" s)) issues)
                                    Icebox
                                    |> div []
                                    |> Just
                            )
                    )
                , column Backlog
                    Nothing
                    (model.iceboxIssues
                        |> Maybe.andThen
                            (\issues ->
                                let
                                    filter which =
                                        (hasLabel "Status: Ready" which)
                                            && (hasNoLabel "Status: In Progress" which)

                                    filteredIssues =
                                        List.filter filter issues

                                    head =
                                        ( Just "😞", text "Just do it" )
                                in
                                    listIssues head True filteredIssues Icebox model Backlog ""
                                        |> Just
                            )
                        |> Maybe.andThen
                            (\htmlNode ->
                                case displayIssuesWithinMilestones (Dict.get model.repo model.milestones) OpenIssue of
                                    Just html ->
                                        div [] [ htmlNode, html ] |> Just

                                    Nothing ->
                                        Nothing
                            )
                    )
                , column Current
                    Nothing
                    (model.currentIssues
                        |> Maybe.andThen
                            (\issues ->
                                displayIssuesGroupedByDate issues Current
                                    |> div []
                                    |> Just
                            )
                    )
                , column Done
                    Nothing
                    (model.closedIssues
                        |> Maybe.andThen
                            (\issues ->
                                listIssues
                                    ( Just "💪", text "We just did it" )
                                    True
                                    issues
                                    Done
                                    model
                                    Done
                                    ""
                                    |> Just
                            )
                        |> Maybe.andThen
                            (\htmlNode ->
                                case displayIssuesWithinMilestones (Dict.get model.repo model.milestones) ClosedIssue of
                                    Just html ->
                                        div [] [ htmlNode, html ] |> Just

                                    Nothing ->
                                        Nothing
                            )
                    )
                ]
    in
        case route of
            Nothing ->
                issuesIndex

            Just r ->
                case r of
                    Story user repo id ->
                        issuesIndex

                    IssuesIndex user repo ->
                        issuesIndex

                    MilestonesIndex user repo ->
                        milestonesIndex

                    Settings user repo ->
                        Html.map SettingsMsgProxy <| viewSettings model


columnTitle : Column -> ( String, String, String )
columnTitle col =
    case col of
        Search ->
            ( "🔎", "Search", "" )

        Icebox ->
            ( "❄", "Icebox", "(keep this place empty)" )

        Backlog ->
            ( "🚥", "Backlog", "(plan all the things via milestones)" )

        Current ->
            ( "🐝", "In progress", "(issues with status 'In Progress')" )

        Done ->
            ( "🎉", "Done", "(closed issues)" )


viewSettings : Model -> Html SettingsMsg
viewSettings model =
    let
        select onSelect values currentValue =
            Html.select [ onInput onSelect ] <| options values currentValue

        options values currentValue =
            values
                |> List.map (option currentValue)

        option current value =
            Html.option [ Attrs.selected <| value == current ] [ text value ]

        settingsBlock title contents =
            div [ style [ ( "background", "#333" ), ( "border", "1px solid #555" ), ( "padding", "5px" ), ( "margin-bottom", "10px" ), ( "max-width", "600px" ) ] ] ((Html.h3 [] [ text title ]) :: contents)
    in
        Html.main_ [ style [ ( "padding", "10px" ), ( "overflow-y", "auto" ), ( "height", "100vh" ), ( "width", "100vw" ) ] ]
            -- default repo
            [ settingsBlock "Default repository"
                [ select ChangeDefaultRepositoryType [ "last visited", "specified" ] model.persistentData.defaultRepositoryType
                , if model.persistentData.defaultRepositoryType == "specified" then
                    Html.input [ Attrs.value model.persistentData.defaultRepository, onInput UpdateDefaultRepository ] []
                  else
                    text ""
                , Html.p [] [ text "this setting controls repository which will be opened when visiting the kanban app" ]
                ]
              -- limit
            , settingsBlock "Limit for 'We just did it'"
                [ select ChangeDoneLimit [ "a day", "a week", "two weeks", "a month" ] model.persistentData.doneLimit
                , Html.p [] [ text "we only pull fresh issues in 'Done' column, here you can configure what is 'fresh'" ]
                ]
              -- focused mode: ignore milestones with no due date
            , settingsBlock "Focus on present, ignore ideas"
                [ Html.label []
                    [ Html.input [ Attrs.checked model.persistentData.powerOfNow, Attrs.type_ "checkbox", onClick IgnoreIdeas ] []
                    , text " ignore milestones with no due date"
                    ]
                , Html.p []
                    [ text <|
                        (if model.persistentData.powerOfNow then
                            "keep this box ticked"
                         else
                            "tick this box"
                        )
                    , text " if you don't want to be bothered by the things that will not happen in the nearest future"
                    ]
                ]
            , settingsBlock "App Version"
                [ Html.strong [] [ text model.version ]
                , Html.p [] [ text "this app is in active development, sometimes you need to refresh app very hard in order to have some old bugs fixed (and possibly grab some new bugs at the same time, sorry), this version number will help you to find whether your cached app version is latest (same as ", Html.a [ Attrs.href "https://github.com/1602/issues-tracker/blob/master/package.json#L4" ] [ text "here" ], text ")." ]
                ]
            ]


listIssuesWithinMilestones : Dict.Dict String ExpandedMilestone -> IssueState -> Date.Date -> Model -> Html Msg
listIssuesWithinMilestones milestones issueState now model =
    let
        pinnedMilestoneNumber =
            model.persistentData.pinnedMilestones
                |> Dict.get model.repo
                |> Maybe.withDefault ""

        milestoneSortingRule ems =
            -- pin to the top
            if ems.milestone.number == pinnedMilestoneNumber then
                0
            else
                case ems.milestone.dueOn of
                    -- milestones without due date go to the bottom
                    Nothing ->
                        1 / 0

                    -- order milestones by closest due date
                    Just date ->
                        date
                            |> Date.toTime
                            |> Time.inSeconds

        renderMilestoneWithIssues expandedMilestone =
            let
                hasIssues =
                    case issueState of
                        OpenIssue ->
                            expandedMilestone.milestone.openIssues > 0

                        ClosedIssue ->
                            expandedMilestone.milestone.closedIssues > 0

                displayIssuesOrLoading head issues =
                    case issues of
                        Just issues ->
                            case issueState of
                                OpenIssue ->
                                    listIssues
                                        head
                                        True
                                        (List.filter (hasNoLabel "Status: In Progress") issues)
                                        Backlog
                                        model
                                        Backlog
                                        expandedMilestone.milestone.number

                                ClosedIssue ->
                                    listIssues
                                        head
                                        True
                                        issues
                                        Done
                                        model
                                        Done
                                        expandedMilestone.milestone.number

                        Nothing ->
                            span [ cellStyle "calc(100% - 8px)" ] [ text "Loading" ]

                issues head =
                    case issueState of
                        OpenIssue ->
                            displayIssuesOrLoading head expandedMilestone.openIssues

                        ClosedIssue ->
                            displayIssuesOrLoading head expandedMilestone.closedIssues

                heading =
                    ( if expandedMilestone.milestone.number == pinnedMilestoneNumber then
                        Just "📌"
                      else
                        Just "🏁"
                      -- , Html.a [ Attrs.target "_blank", Attrs.href expandedMilestone.milestone.htmlUrl ] [ text <| expandedMilestone.milestone.title ++ " " ]
                    , Html.span
                        [ Attrs.tabindex 10
                        , Attrs.class "no-outline"
                        , Attrs.id <| "milestone-" ++ expandedMilestone.milestone.number
                        , onClick <| PinMilestone expandedMilestone.milestone.number
                        , style [ ( "cursor", "pointer" ) ]
                        ]
                        [ text <|
                            expandedMilestone.milestone.title
                                ++ (case expandedMilestone.milestone.dueOn of
                                        Just date ->
                                            if (Date.toTime date |> Time.inSeconds) < (Date.toTime now |> Time.inSeconds) then
                                                " overdue"
                                            else
                                                " due in " ++ (Distance.inWords now date)

                                        Nothing ->
                                            " (no due date)"
                                   )
                        ]
                    )
            in
                if hasIssues || True then
                    issues heading
                else
                    text ""
    in
        milestones
            |> Dict.values
            |> List.sortBy milestoneSortingRule
            |> List.map renderMilestoneWithIssues
            |> div []


buttonStyle : List ( String, String )
buttonStyle =
    [ ( "margin-right", "15px" )
    , ( "margin-top", "9px" )
    , ( "border", "0px" )
    , ( "background", "#eee" )
    , ( "color", "#222" )
    , ( "cursor", "pointer" )
    , ( "box-shadow", "0px 0px 0px 5px rgba(5,5,5,0.2)" )
    , ( "border-radius", "1px" )
    , ( "font-family", "Fira Code, Iosevka, menlo, monospace" )
    ]


listIssues : ( Maybe String, Html Msg ) -> Bool -> List Issue -> Column -> Model -> Column -> String -> Html Msg
listIssues ( icon, head ) allowAdd issues col model addto milestoneNumber =
    let
        lockedIssueNumber =
            model.lockedIssueNumber

        button issue title =
            Html.button
                [ style buttonStyle
                , onClick (IssueAction issue title)
                ]
                [ text title ]

        getTypeClass issue =
            issue.labels
                |> List.foldl
                    (\l res ->
                        if l.name == "Type: Bug" || l.name == "bug" then
                            Just "bug"
                        else if l.name == "Type: Feature" || l.name == "enhancement" then
                            Just "feature"
                        else if l.name == "Type: Research" then
                            Just "research"
                        else
                            res
                    )
                    Nothing
                |> Maybe.withDefault "idea"

        getTypeIcon issue =
            issue.labels
                |> List.foldl
                    (\l res ->
                        if l.name == "Type: Bug" then
                            Just "🐞 "
                        else if l.name == "Type: Feature" then
                            Just "🔨 "
                        else if l.name == "Type: Research" then
                            Just "🔎 "
                        else
                            res
                    )
                    Nothing
                |> Maybe.withDefault "💡 "

        getPriorityColor issue =
            issue.labels
                |> List.foldl
                    (\l res ->
                        if l.name == "Priority: High" then
                            Just "rgb(240,140,52)"
                        else if l.name == "Priority: Critical" then
                            Just "#fd4242"
                        else
                            res
                    )
                    Nothing
                |> Maybe.withDefault "grey"

        getStoryClass issue =
            if issue.number == model.highlightStory then
                Attrs.class "story selected"
            else
                Attrs.class "story not-selected"

        milestone =
            case List.head issues of
                Just issue ->
                    issue.milestone

                Nothing ->
                    Nothing
    in
        issues
            |> List.filter
                (\issue ->
                    model.filterStoriesBy == "" || String.contains (String.toLower model.filterStoriesBy) (String.toLower issue.title)
                )
            |> List.map
                (\issue ->
                    div
                        [ getStoryClass issue
                        , Attrs.tabindex 1
                        , Attrs.id <| "story-" ++ issue.number
                        , style
                            [ if issue.number == lockedIssueNumber then
                                ( "filter", "grayscale(0.5) blur(2px)" )
                              else
                                ( "filter", "none" )
                            ]
                        ]
                        [ span [ cellStyle "calc(100% - 4px)" ]
                            [ span [ Attrs.class <| "icon " ++ (getTypeClass issue) ] [ text <| getTypeIcon issue ]
                            , Html.a [ Attrs.href issue.htmlUrl, Attrs.target "_blank" ] [ text <| "#" ++ issue.number ]
                            , Html.a
                                [ style [ ( "color", getPriorityColor issue ), ( "cursor", "pointer" ) ]
                                  --, onClick <| SelectStory issue
                                , if model.highlightStory == issue.number then
                                    Attrs.href <| "#/" ++ model.repo ++ "/stories"
                                  else
                                    Attrs.href <| "#/" ++ model.repo ++ "/stories/" ++ issue.number
                                ]
                                [ text <| " " ++ issue.title ++ " " ]
                            , Html.i [ style [ ( "color", "darkgrey" ) ] ]
                                (if List.length issue.assignees == 0 then
                                    [ text "(unassigned)" ]
                                 else
                                    issue.assignees
                                        |> List.map
                                            (\s ->
                                                Html.img
                                                    [ src s.avatar, Attrs.width 20, style [ ( "vertical-align", "middle" ) ] ]
                                                    []
                                            )
                                )
                            , if issue.number == model.highlightStory then
                                div []
                                    [ Html.p [] [ ghMd model.repo issue.description ]
                                    , Html.p []
                                        [ text "created by "
                                        , text issue.creator.login
                                        , text " "
                                        , text <| Distance.inWords model.now issue.createdAt
                                        , text " ago"
                                        ]
                                    , if issue.updatedAt /= issue.createdAt then
                                        Html.p []
                                            [ text "last update "
                                            , text <| Distance.inWords model.now issue.updatedAt
                                            , text " ago"
                                            ]
                                      else
                                        text ""
                                    ]
                              else
                                text ""
                            , div [ Attrs.class "buttons" ] <|
                                case col of
                                    Search ->
                                        []

                                    Backlog ->
                                        [ button issue "unplan", button issue "start" ]

                                    Icebox ->
                                        [ button issue "plan"
                                        , button issue <|
                                            (if hasLabel "Status: Ready" issue then
                                                "ice"
                                             else
                                                "just do it"
                                            )
                                        , if hasLabel "Status: Ready" issue then
                                            button issue "start"
                                          else
                                            text ""
                                        ]

                                    Current ->
                                        [ button issue "unstart"
                                        , button issue "finish"
                                        ]

                                    Done ->
                                        [ button issue "restart" ]
                            ]
                        ]
                )
            |> (\list ->
                    if allowAdd then
                        case col of
                            Done ->
                                list

                            _ ->
                                (if model.addIssueToColumn == addto && model.addIssueToMilestone == milestoneNumber then
                                    (Html.form [ cellExStyle [ ( "background", "#333" ) ], Html.Events.onSubmit <| CreateStory addto ]
                                        [ Html.input [ Attrs.id "create-story", style [ ( "width", "90%" ) ], onInput EditNewStoryTitle, Attrs.value model.newIssueTitle ] []
                                        , Html.button [ style buttonStyle ] [ text "Add" ]
                                        , Html.span [ style [ ( "cursor", "pointer" ) ], onClick <| ShowIssueCreationForm Done "" ] [ text "Cancel" ]
                                        ]
                                    )
                                        :: list
                                 else
                                    list
                                )
                    else
                        list
               )
            |> (\list ->
                    if List.isEmpty list && model.filterStoriesBy /= "" then
                        []
                    else
                        [ div
                            [ style
                                [ ( "background", "#111" )
                                , ( "padding", "2px" )
                                  -- , ( "width", "408px" )
                                , ( "margin-top", "4px" )
                                , ( "margin-right", "4px" )
                                ]
                            ]
                            ([ Html.strong []
                                [ Html.span
                                    [ cellExStyle
                                        [ ( "width", "calc(100% - 4px)" )
                                        , ( "background", "#111" )
                                        , ( "padding", "2px" )
                                        ]
                                    ]
                                    [ text <| (Maybe.withDefault "" icon) ++ " "
                                    , head
                                    , if col == Done || not allowAdd || model.addIssueToColumn == addto && model.addIssueToMilestone == milestoneNumber then
                                        if allowAdd && col /= Done then
                                            span
                                                [ style
                                                    [ ( "position", "absolute" )
                                                    , ( "right", "0px" )
                                                    , ( "width", "20px" )
                                                    , ( "height", "20px" )
                                                    , ( "background", "#111" )
                                                    , ( "line-height", "20px" )
                                                    , ( "text-align", "center" )
                                                    , ( "cursor", "pointer" )
                                                    ]
                                                , onClick <| ShowIssueCreationForm Done ""
                                                ]
                                                [ text " - " ]
                                        else
                                            text ""
                                      else
                                        span
                                            [ style
                                                [ ( "position", "absolute" )
                                                , ( "right", "0px" )
                                                , ( "width", "20px" )
                                                , ( "height", "20px" )
                                                , ( "background", "#111" )
                                                , ( "line-height", "20px" )
                                                , ( "text-align", "center" )
                                                , ( "cursor", "pointer" )
                                                ]
                                            , onClick <| ShowIssueCreationForm addto milestoneNumber
                                            ]
                                            [ text " + " ]
                                    ]
                                ]
                             ]
                                ++ list
                            )
                        ]
               )
            |> div []


intToDate : Int -> Date.Date
intToDate ms =
    ms
        |> toFloat
        |> (\ms -> Time.millisecond * ms)
        |> Date.fromTime


cellStyle : String -> Html.Attribute msg
cellStyle width =
    style
        [ ( "padding", "10px" )
        , ( "margin", "2px" )
        , ( "vertical-align", "top" )
        , ( "width", width )
          -- , ( "font-family", "monospace" )
        , ( "overflow", "hidden" )
        , ( "text-overflow", "ellipsis" )
        , ( "display", "inline-block" )
        , ( "background", "rgba(255,255,255,0.1)" )
        , ( "position", "relative" )
        , ( "box-sizing", "border-box" )
        ]


cellExStyle : List ( String, String ) -> Html.Attribute msg
cellExStyle list =
    style
        ([ ( "padding", "10px" )
         , ( "margin", "2px" )
         , ( "vertical-align", "top" )
         , ( "width", "calc(100% - 4px)" )
           -- , ( "font-family", "monospace" )
         , ( "overflow", "hidden" )
         , ( "text-overflow", "ellipsis" )
         , ( "display", "inline-block" )
         , ( "background", "rgba(255,255,255,0.1)" )
         , ( "position", "relative" )
         , ( "box-sizing", "border-box" )
         , ( "line-height", "22px" )
         ]
            ++ list
        )


shortenUuid : String -> String
shortenUuid uuid =
    (String.left 9 uuid) ++ "..." ++ (String.right 4 uuid)


formField : String -> Html msg -> Html msg
formField label control =
    div [ style [ ( "padding", "10px" ) ] ]
        [ Html.label [] [ text label, div [ style [ ( "margin-top", "5px" ) ] ] [ control ] ] ]


textareaStyle : Html.Attribute msg
textareaStyle =
    Attrs.style
        [ ( "width", "100%" )
        , ( "height", "400px" )
        , ( "padding", "5px" )
        , ( "color", "#0F0" )
        ]


viewNavigation : Maybe User -> Model -> Html Msg
viewNavigation user model =
    let
        showColumns =
            model.persistentData.columns

        issuesSubnav =
            []
                |> reopeningColumnButton Done showColumns
                |> reopeningColumnButton Current showColumns
                |> reopeningColumnButton Backlog showColumns
                |> reopeningColumnButton Icebox showColumns
                |> reopeningColumnButton Search showColumns
                |> div []

        activePage =
            parseHash model.location

        isSettingsActive =
            case activePage of
                Just (Settings _ _) ->
                    True

                _ ->
                    False

        subNav =
            case activePage of
                Just (Story _ _ _) ->
                    issuesSubnav

                Just (IssuesIndex _ _) ->
                    issuesSubnav

                _ ->
                    text ""

        menuListItemStyle isActive =
            style
                [ ( "list-style", "none" )
                , ( "display", "block" )
                , ( "padding", "0px" )
                , ( "margin", "0" )
                , ( "margin-bottom", "10px" )
                , ( "font-weight", "700" )
                , ( "background"
                  , if isActive then
                        "rgb(246,246,247)"
                    else
                        "rgba(255,255,255, 0.1)"
                  )
                , ( "color", "black" )
                , ( "font-size", "20px" )
                , ( "width", "30px" )
                , ( "height", "30px" )
                , ( "text-align", "center" )
                  -- , ( "height", "30px" )
                ]

        viewLink src childNode location =
            let
                repo =
                    location.hash
                        |> String.split "/"
                        |> List.drop 1
                        |> List.take 2
                        |> String.join "/"

                isActive =
                    location.hash
                        |> String.split "/"
                        |> List.drop 3
                        |> String.join "/"
                        |> String.startsWith src

                url =
                    "#/" ++ repo ++ "/" ++ src

                link =
                    if isActive then
                        childNode
                    else
                        Html.a
                            [ Attrs.href url ]
                            [ childNode ]
            in
                Html.li [ menuListItemStyle isActive ] [ link ]
    in
        div [ style [ ( "height", "100vh" ), ( "width", "41px" ), ( "background-color", "#1d1d1d" ), ( "padding", "5px" ) ] ]
            [ Html.ul
                [ style
                    [ ( "position", "absolute" )
                    , ( "bottom", "0px" )
                    , ( "vertical-align", "middle" )
                    , ( "margin", "0" )
                    , ( "padding", "0" )
                    ]
                ]
                [ case user of
                    Nothing ->
                        text ""

                    Just user ->
                        Html.li [ menuListItemStyle isSettingsActive ]
                            [ Html.a
                                [ Attrs.href <| "#/" ++ model.repo ++ "/settings"
                                , style
                                    [ ( "display", "inline-block" )
                                    , ( "vertical-align", "middle" )
                                    ]
                                ]
                                [ img
                                    [ src user.avatar
                                    , Attrs.width 24
                                    , style [ ( "vertical-align", "middle" ) ]
                                    ]
                                    []
                                ]
                            ]
                ]
            , [ viewLink "stories" (text "🔬")
              , viewLink "milestones" (text "🔭")
              ]
                |> List.map (\s -> s model.location)
                |> Html.ul
                    [ style
                        [ ( "list-style", "none" )
                        , ( "display", "inline-block" )
                        , ( "margin", "0" )
                        , ( "padding", "0" )
                        , ( "margin-bottom", "20px" )
                        ]
                    ]
              {-
                 , text " Show stories: "
                 , Html.select
                     [ onInput ChangeFilter
                     , Attrs.value
                         (case model.filter of
                             AssignedTo _ ->
                                 "assigned to me"

                             CreatedBy _ ->
                                 "created by me"

                             HasMentionOf _ ->
                                 "mentioning me"

                             All ->
                                 "all"
                         )
                     ]
                     [ Html.option [] [ text "all" ]
                     , Html.option [] [ text "assigned to me" ]
                     , Html.option [] [ text "created by me" ]
                     , Html.option [] [ text "mentioning me" ]
                     ]
                     -
              -}
            , subNav
              --, text " Filter stories: "
              --, Html.input [ Attrs.value model.filterStoriesBy, onInput FilterStories ] []
            ]


reopeningColumnButton : Column -> List Column -> List (Html Msg) -> List (Html Msg)
reopeningColumnButton col showColumns list =
    let
        ( icon, title, _ ) =
            columnTitle col
    in
        if List.member col showColumns then
            (Html.button
                [ style
                    (buttonStyle
                        |> List.filter (\( s, _ ) -> s /= "margin-top")
                        |> (++)
                            [ ( "font-size", "18px" )
                            , ( "height", "30px" )
                            , ( "width", "30px" )
                            , ( "margin-top", "10px" )
                            , ( "transition", "filter 0.1s, background 0.1s" )
                            ]
                    )
                , onClick <| HideColumn col
                ]
                [ text <| icon ]
            )
                :: list
        else
            (Html.button
                [ style
                    (buttonStyle
                        |> List.filter
                            (\( s, _ ) ->
                                s /= "margin-top" && s /= "background" && s /= "color"
                            )
                        |> (++)
                            [ ( "font-size", "18px" )
                            , ( "background", "rgba(40,40,40,1)" )
                            , ( "color", "#bbb" )
                            , ( "transition", "filter 0.1s, background 0.1s" )
                            , ( "filter", "grayscale(0.9) brightness(50%)" )
                            , ( "height", "30px" )
                            , ( "width", "30px" )
                            , ( "margin-top", "10px" )
                            ]
                    )
                , onClick <| ReopenColumn col
                ]
                [ text <| icon ]
            )
                :: list
