port module Main exposing (..)

import Route exposing (Route, Route(..), parseHash)
import Models exposing (..)
import Messages exposing (..)
import Services exposing (..)
import Dict
import List
import Html exposing (Html, span, text, img, div)
import Navigation exposing (programWithFlags, Location)
import Http exposing (Error, Response)
import Date
import Time
import Task
import Date.Distance as Distance
import Html.Attributes as Attrs exposing (style, class, attribute, src)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Markdown
import Dom


-- import Base exposing (..)
-- import Cruft exposing (clipboardIcon)
-- import Date.Extra
-- import Json.Decode as Decode exposing (field)
-- import Json.Encode as Encode
-- import List.Extra exposing (find)
-- APP


main : Program PersistedData Model Msg
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


type alias Model =
    { user : Maybe User
    , token : String
    , accessToken : Maybe String
    , repo : String
    , location : Location
    , now : Date.Date
    , error : Maybe String
    , currentIssues : Maybe (List Issue)
    , iceboxIssues : Maybe (List Issue)
    , closedIssues : Maybe (List Issue)
    , milestones : Maybe (Dict.Dict String ExpandedMilestone)
    , pickMilestoneForIssue : Maybe Issue
    , lockedIssueNumber : String
    , highlightStory : String
    , newMilestoneTitle : String
    , newIssueTitle : String
    , needFocus : Bool
    , addIssueToColumn : Column
    , addIssueToMilestone : String
    }


init : PersistedData -> Location -> ( Model, Cmd Msg )
init persistentData location =
    let
        page =
            parseHash location

        highlightStory =
            case page of
                Just (Story _ _ s) ->
                    s

                _ ->
                    ""

        model =
            Model
                Nothing
                ""
                persistentData.accessToken
                (extractRepo location.hash)
                location
                (Date.fromTime <| Time.millisecond * (toFloat 0))
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                ""
                highlightStory
                ""
                ""
                (highlightStory /= "")
                -- needs focus
                Done
                ""

        -- needFocus
    in
        model
            ! ([ Task.perform CurrentDate Date.now
               , case persistentData.accessToken of
                    Just accessToken ->
                        fetchUser accessToken

                    --fetchClients user.secretKey
                    Nothing ->
                        Cmd.none
               , case page of
                    Nothing ->
                        Navigation.modifyUrl <| "#/" ++ model.repo ++ "/stories"

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


aboutToLoadResource : Location -> Model -> Model
aboutToLoadResource loc model =
    let
        page =
            parseHash loc
    in
        case page of
            Just (IssuesIndex user repo) ->
                { model | highlightStory = "" }

            Just (Story _ _ s) ->
                { model | highlightStory = s }

            _ ->
                { model | highlightStory = "" }


loadAllIssues : String -> String -> List (Cmd Msg)
loadAllIssues repo accessToken =
    [ fetchIssues repo accessToken Current
    , fetchIssues repo accessToken Icebox
    , fetchIssues repo accessToken Done
    , fetchMilestones repo accessToken
    ]


loadResource : Model -> List (Cmd Msg)
loadResource model =
    case model.currentIssues of
        Nothing ->
            case model.user of
                Just user ->
                    case model.accessToken of
                        Just token ->
                            case parseHash model.location of
                                Just (Story user repo id) ->
                                    loadAllIssues model.repo token

                                Just (IssuesIndex user repo) ->
                                    loadAllIssues model.repo token

                                Just (MilestonesIndex user repo) ->
                                    loadAllIssues model.repo token

                                Nothing ->
                                    loadAllIssues model.repo token

                        Nothing ->
                            []

                Nothing ->
                    []

        _ ->
            []


port googleAuth : (String -> msg) -> Sub msg


port saveData : PersistedData -> Cmd msg


port clipboard : String -> Cmd msg



-- UPDATE


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- TODO: separate actions which require user
    case msg of
        NoOp ->
            model ! []

        ShowIssueCreationForm col msn ->
            { model | addIssueToColumn = col, addIssueToMilestone = msn } ! []

        EditNewStoryTitle s ->
            { model | newIssueTitle = s } ! []

        CreateStory col milestone ->
            case model.accessToken of
                Just token ->
                    { model | newIssueTitle = "" }
                        ! (if model.newIssueTitle /= "" then
                            [ createIssue model.repo
                                token
                                ([ ( "title", Encode.string model.newIssueTitle )
                                 , ( "body", Encode.string "" )
                                 , ( "labels"
                                   , Encode.list <|
                                        (case col of
                                            Backlog ->
                                                [ Encode.string "Status: Ready" ]

                                            Current ->
                                                [ Encode.string "Status: In Progress" ]

                                            _ ->
                                                []
                                        )
                                   )
                                 , ( "milestone"
                                   , case milestone of
                                        Just ms ->
                                            ms.number |> String.toInt |> Result.withDefault 0 |> Encode.int

                                        Nothing ->
                                            Encode.null
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
                                    |> Encode.object
                                )
                                (StoryCreated col milestone)
                            ]
                           else
                            []
                          )

                Nothing ->
                    model ! []

        StoryCreated col milestone result ->
            case result of
                Err error ->
                    { model | error = toString error |> Just } ! []

                Ok _ ->
                    case model.accessToken of
                        Just token ->
                            model
                                ! [ case col of
                                        Backlog ->
                                            case milestone of
                                                Just ms ->
                                                    fetchMilestoneIssues model.repo token IssueOpen ms

                                                Nothing ->
                                                    fetchIssues model.repo token Icebox

                                        _ ->
                                            fetchIssues model.repo token col
                                  ]

                        Nothing ->
                            model ! []

        UrgentIssueAdded result ->
            model ! [ fetchIssues model.repo (Maybe.withDefault "" model.accessToken) Icebox ]

        StoryFocused ->
            { model | needFocus = False } ! []

        EditNewMilestoneTitle s ->
            { model | newMilestoneTitle = s } ! []

        CreateNewMilestone ->
            { model | newMilestoneTitle = "" }
                ! [ createMilestone model.repo model.newMilestoneTitle model.accessToken
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
                    { model | error = Just (toString e), accessToken = Nothing, user = Nothing } ! []

        SaveAccessToken ->
            let
                updatedModel =
                    { model | accessToken = Just model.token }
            in
                if model.token == "" then
                    model ! [ Navigation.load "https://github.com/settings/tokens" ]
                else
                    updatedModel ! (fetchUser model.token :: ((saveData <| PersistedData (Just model.token)) :: (loadResource updatedModel)))

        CurrentDate now ->
            { model | now = now } ! []

        CurrentTime now ->
            { model | now = Date.fromTime now }
                ! (case model.accessToken of
                    Just token ->
                        loadAllIssues model.repo token

                    Nothing ->
                        []
                  )

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
                repo =
                    extractRepo location.hash

                issues =
                    if repo == model.repo then
                        model.currentIssues
                    else
                        Nothing

                milestones =
                    if repo == model.repo then
                        model.milestones
                    else
                        Nothing

                updatedModel =
                    ({ model
                        | location = location
                        , repo = repo
                        , currentIssues = issues
                        , milestones = milestones
                     }
                        |> aboutToLoadResource location
                    )
            in
                updatedModel ! loadResource updatedModel

        MilestoneIssuesLoaded num issueState result ->
            case result of
                Err error ->
                    { model | error = Just (toString error) } ! []

                Ok issues ->
                    let
                        updatedModel =
                            { model
                                | milestones =
                                    Just
                                        (Maybe.withDefault Dict.empty model.milestones
                                            |> Dict.update num
                                                (\s ->
                                                    case s of
                                                        Just ms ->
                                                            case issueState of
                                                                IssueOpen ->
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

                                                                IssueClosed ->
                                                                    let
                                                                        m =
                                                                            ms.milestone

                                                                        updatedMilestone =
                                                                            { m | closedIssues = List.length issues }
                                                                    in
                                                                        Just
                                                                            { ms
                                                                                | closedIssues = Just issues
                                                                                , milestone = m
                                                                            }

                                                        Nothing ->
                                                            Nothing
                                                )
                                        )
                            }

                        mss =
                            case updatedModel.milestones of
                                Just milestones ->
                                    Dict.values milestones

                                Nothing ->
                                    []

                        loaded =
                            List.foldl
                                (\ms res ->
                                    case issueState of
                                        IssueClosed ->
                                            case ms.closedIssues of
                                                Nothing ->
                                                    if ms.milestone.closedIssues > 0 then
                                                        res
                                                    else
                                                        res + 1

                                                Just _ ->
                                                    res + 1

                                        IssueOpen ->
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

        LoadMilestones result ->
            case result of
                Err error ->
                    { model | error = Just (toString error) } ! []

                Ok milestones ->
                    { model
                        | milestones =
                            milestones
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
                                    (model.milestones |> Maybe.withDefault Dict.empty)
                                |> Just
                        , error = Nothing
                    }
                        ! (case model.accessToken of
                            Just token ->
                                (milestones
                                    |> List.filter (\ms -> ms.openIssues > 0)
                                    |> List.map (fetchMilestoneIssues model.repo token IssueOpen)
                                )
                                    ++ (milestones
                                            |> List.filter (\ms -> ms.closedIssues > 0)
                                            |> List.map (fetchMilestoneIssues model.repo token IssueClosed)
                                       )

                            Nothing ->
                                []
                          )

        MilestoneCreated result ->
            case result of
                Ok _ ->
                    { model | error = Nothing }
                        ! [ fetchMilestones model.repo (Maybe.withDefault "" model.accessToken)
                          ]

                Err e ->
                    { model | error = toString e |> Just } ! []

        IssuesLoaded column result ->
            case result of
                Ok issues ->
                    case column of
                        Current ->
                            { model | currentIssues = Just issues, error = Nothing }
                                ! (if model.needFocus then
                                    [ focus model.location ]
                                   else
                                    []
                                  )

                        Icebox ->
                            { model | iceboxIssues = Just issues, error = Nothing }
                                ! (if model.needFocus then
                                    [ focus model.location ]
                                   else
                                    []
                                  )

                        Done ->
                            { model | closedIssues = Just issues, error = Nothing }
                                ! (if model.needFocus then
                                    [ focus model.location ]
                                   else
                                    []
                                  )

                        _ ->
                            model ! []

                Err e ->
                    { model | error = toString e |> Just } ! []

        CopyText str ->
            model ! [ clipboard str ]

        UnsetMilestone m result ->
            { model | lockedIssueNumber = "" }
                ! (case model.accessToken of
                    Just token ->
                        [ fetchMilestoneIssues model.repo token IssueOpen m
                        , fetchIssues model.repo token Icebox
                        ]

                    Nothing ->
                        []
                  )

        SetMilestone issue milestone ->
            case model.accessToken of
                Just token ->
                    { model
                        | pickMilestoneForIssue = Nothing
                        , lockedIssueNumber = issue.number
                    }
                        ! [ updateIssueWith model.repo
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
                                token
                                (MilestoneSet milestone)
                          ]

                Nothing ->
                    model ! []

        MilestoneSet m result ->
            { model | lockedIssueNumber = "" }
                ! (case model.accessToken of
                    Just token ->
                        [ fetchMilestoneIssues model.repo token IssueOpen m
                        , fetchIssues model.repo token Icebox
                        ]

                    Nothing ->
                        []
                  )

        IssueRestarted m result ->
            { model | lockedIssueNumber = "" }
                ! (case model.accessToken of
                    Just token ->
                        case m of
                            Just milestone ->
                                [ fetchMilestoneIssues model.repo token IssueClosed milestone
                                , fetchIssues model.repo token Current
                                ]

                            Nothing ->
                                [ fetchIssues model.repo token Done
                                , fetchIssues model.repo token Current
                                ]

                    Nothing ->
                        []
                  )

        IssueStarted milestone result ->
            { model | lockedIssueNumber = "" }
                ! (case model.accessToken of
                    Just token ->
                        case milestone of
                            Just m ->
                                [ fetchMilestoneIssues model.repo token IssueOpen m
                                , fetchIssues model.repo token Current
                                ]

                            Nothing ->
                                [ fetchIssues model.repo token Current
                                , fetchIssues model.repo token Icebox
                                ]

                    Nothing ->
                        []
                  )

        IssueFinished m result ->
            { model | lockedIssueNumber = "" }
                ! (case model.accessToken of
                    Just token ->
                        case m of
                            Just m ->
                                [ fetchMilestoneIssues model.repo token IssueClosed m
                                , fetchIssues model.repo token Current
                                ]

                            Nothing ->
                                [ fetchIssues model.repo token Current
                                , fetchIssues model.repo token Done
                                ]

                    Nothing ->
                        []
                  )

        DismissPlanningIssue ->
            { model | pickMilestoneForIssue = Nothing } ! []

        IssueAction issue action ->
            case model.accessToken of
                Just token ->
                    case action of
                        "unplan" ->
                            case issue.milestone of
                                Just m ->
                                    { model | lockedIssueNumber = issue.number }
                                        ! [ UnsetMilestone m
                                                |> updateIssue model.repo issue token
                                          ]

                                Nothing ->
                                    model ! []

                        "start" ->
                            case model.user of
                                Just user ->
                                    { model | lockedIssueNumber = issue.number }
                                        ! [ updateIssueWith model.repo
                                                issue.number
                                                (Encode.object
                                                    [ ( "labels"
                                                      , issue.labels
                                                            |> List.map .name
                                                            |> (::) "Status: In Progress"
                                                            |> List.map Encode.string
                                                            |> Encode.list
                                                      )
                                                    , ( "assignees"
                                                      , [ Encode.string user.login ] |> Encode.list
                                                      )
                                                    ]
                                                )
                                                token
                                                (IssueStarted issue.milestone)
                                          ]

                                Nothing ->
                                    model ! []

                        "finish" ->
                            { model | lockedIssueNumber = issue.number }
                                ! [ updateIssueWith model.repo
                                        issue.number
                                        (Encode.object
                                            [ ( "labels"
                                              , issue.labels
                                                    |> List.map .name
                                                    |> List.filter (\s -> s /= "Status: In Progress")
                                                    |> (::) "Status: Completed"
                                                    |> List.map Encode.string
                                                    |> Encode.list
                                              )
                                            , ( "state", Encode.string "closed" )
                                            ]
                                        )
                                        token
                                        (IssueFinished issue.milestone)
                                  ]

                        "reopen" ->
                            { model | lockedIssueNumber = issue.number }
                                ! [ updateIssueWith model.repo
                                        issue.number
                                        (Encode.object
                                            [ ( "labels"
                                              , issue.labels
                                                    |> List.map .name
                                                    |> List.filter (\s -> s /= "Status: In Progress")
                                                    |> (::) "Status: In Progress"
                                                    |> List.map Encode.string
                                                    |> Encode.list
                                              )
                                            , ( "state", Encode.string "open" )
                                            ]
                                        )
                                        token
                                        (IssueRestarted issue.milestone)
                                  ]

                        "unstart" ->
                            { model | lockedIssueNumber = issue.number }
                                ! [ updateIssueWith model.repo
                                        issue.number
                                        (Encode.object
                                            [ ( "labels"
                                              , issue.labels
                                                    |> List.map .name
                                                    |> List.filter (\s -> s /= "Status: In Progress")
                                                    |> List.map Encode.string
                                                    |> Encode.list
                                              )
                                            ]
                                        )
                                        token
                                        (IssueStarted issue.milestone)
                                  ]

                        "plan" ->
                            { model | pickMilestoneForIssue = Just issue } ! []

                        "put on ice" ->
                            model
                                ! [ updateIssueWith model.repo
                                        issue.number
                                        (Encode.object
                                            [ ( "labels"
                                              , issue.labels
                                                    |> List.map .name
                                                    |> List.filter (\s -> s /= "Status: Ready")
                                                    |> List.map Encode.string
                                                    |> Encode.list
                                              )
                                            ]
                                        )
                                        token
                                        UrgentIssueAdded
                                  ]

                        "just do it" ->
                            model
                                ! [ updateIssueWith model.repo
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
                                        token
                                        UrgentIssueAdded
                                  ]

                        _ ->
                            model ! []

                Nothing ->
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
        [ Time.every (60 * Time.second) CurrentTime
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
                            ]
                        ]
                        [ text error ]

                Nothing ->
                    text ""
    in
        case model.user of
            Just user ->
                div []
                    [ viewTopbar user model.location
                    , viewPage user model <| parseHash model.location
                    , error
                    , case model.pickMilestoneForIssue of
                        Just issue ->
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

                        Nothing ->
                            text ""
                    ]

            Nothing ->
                case model.accessToken of
                    Nothing ->
                        case model.error of
                            Just err ->
                                div [] [ text err ]

                            Nothing ->
                                div []
                                    [ text "cheers. visit "
                                    , Html.a [ Attrs.href "https://github.com/settings/tokens" ] [ text "https://github.com/settings/tokens" ]
                                    , text " (we need 'repo' access granted to see all private repositories)"
                                    , Html.br [] []
                                    , text "and fill this input "
                                    , Html.input [ onInput EditAccessToken ] []
                                    , Html.button [ onClick SaveAccessToken ] [ text "then press this button" ]
                                    ]

                    Just _ ->
                        div [] [ text "Bear with me. I'm currently loading user information..." ]


viewPage : User -> Model -> Maybe Route -> Html Msg
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
                                        IssueClosed ->
                                            case ms.closedIssues of
                                                Nothing ->
                                                    if ms.milestone.closedIssues > 0 then
                                                        res
                                                    else
                                                        res + 1

                                                Just _ ->
                                                    res + 1

                                        IssueOpen ->
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
                            listIssuesWithinMilestones milestones issueState model.now model
                        else
                            span [ cellStyle "400px" ] [ text <| "Loading milestones (" ++ (toString loaded) ++ " of " ++ (toString total) ++ ")..." ]

                Nothing ->
                    span [ cellStyle "400px" ] [ text "Loading milestones..." ]

        displayIssues head filter issues col addto milestoneNumber =
            case issues of
                Just issues ->
                    listIssues head (List.filter filter issues) col model addto milestoneNumber

                Nothing ->
                    span [ cellStyle "400px" ] [ text "Loading..." ]

        milestonesIndex =
            case model.milestones of
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

        issuesIndex =
            Html.main_
                [ style
                    [ ( "display", "flex" )
                    , ( "width", "100%" )
                    ]
                ]
                [ Html.section []
                    [ Html.h3 [] [ text "❄ Icebox ", Html.small [] [ text "(keep this place empty)" ] ]
                    , displayIssues Nothing
                        (hasNoLabel "Status: Ready")
                        model.iceboxIssues
                        Icebox
                        Icebox
                        ""
                    ]
                , Html.section []
                    [ Html.h3 [] [ text "🚥 Backlog ", Html.small [] [ text "(plan all the things via milestones)" ] ]
                    , displayIssues
                        ("😞 Just do it"
                            |> text
                            |> (\s -> [ s ])
                            |> Html.strong [ style [ ( "color", "white" ), ( "line-height", "22px" ), ( "font-size", "14px" ) ] ]
                            |> Just
                        )
                        (\which -> (hasLabel "Status: Ready" which) && (hasNoLabel "Status: In Progress" which))
                        model.iceboxIssues
                        Icebox
                        Backlog
                        ""
                    , displayIssuesWithinMilestones model.milestones IssueOpen
                    ]
                , Html.section []
                    [ Html.h3 [] [ text "🐝 In progress ", Html.small [] [ text "(issues with status 'In Progress')" ] ]
                    , displayIssues Nothing (\_ -> True) model.currentIssues Current Current ""
                    ]
                , Html.section []
                    [ Html.h3 [] [ text "🎉 Done ", Html.small [] [ text "(closed issues)" ] ]
                    , displayIssues
                        ("💪 We just did it"
                            |> text
                            |> (\s -> [ s ])
                            |> Html.strong []
                            |> Just
                        )
                        (\_ -> True)
                        model.closedIssues
                        Done
                        Done
                        ""
                    , displayIssuesWithinMilestones model.milestones IssueClosed
                    ]
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


listIssuesWithinMilestones : Dict.Dict String ExpandedMilestone -> IssueState -> Date.Date -> Model -> Html Msg
listIssuesWithinMilestones milestones issueState now model =
    milestones
        |> Dict.values
        |> List.sortBy
            (\ems ->
                case ems.milestone.dueOn of
                    Nothing ->
                        1 / 0

                    Just date ->
                        date
                            |> Date.toTime
                            |> Time.inSeconds
            )
        |> List.map
            (\expandedMilestone ->
                let
                    hasIssues =
                        case issueState of
                            IssueOpen ->
                                expandedMilestone.milestone.openIssues > 0

                            IssueClosed ->
                                expandedMilestone.milestone.closedIssues > 0

                    displayIssuesOrLoading head issues =
                        case issues of
                            Just issues ->
                                case issueState of
                                    IssueOpen ->
                                        listIssues
                                            head
                                            (List.filter (hasNoLabel "Status: In Progress") issues)
                                            Backlog
                                            model
                                            Backlog
                                            expandedMilestone.milestone.number

                                    IssueClosed ->
                                        listIssues
                                            head
                                            issues
                                            Done
                                            model
                                            Done
                                            expandedMilestone.milestone.number

                            Nothing ->
                                span [ cellStyle "400px" ] [ text "Loading" ]

                    issues head =
                        case issueState of
                            IssueOpen ->
                                displayIssuesOrLoading (Just head) expandedMilestone.openIssues

                            IssueClosed ->
                                displayIssuesOrLoading (Just head) expandedMilestone.closedIssues

                    heading =
                        span []
                            [ Html.strong [ style [ ( "color", "yellowgreen" ), ( "line-height", "22px" ), ( "font-size", "14px" ) ] ]
                                [ text <| "🏁 "
                                , Html.a [ Attrs.target "_blank", Attrs.href expandedMilestone.milestone.htmlUrl ] [ text <| expandedMilestone.milestone.title ++ " " ]
                                ]
                            , case expandedMilestone.milestone.dueOn of
                                Just date ->
                                    if (Date.toTime date |> Time.inSeconds) < (Date.toTime now |> Time.inSeconds) then
                                        text " overdue"
                                    else
                                        text <| " due in " ++ (Distance.inWords now date)

                                Nothing ->
                                    text " (no due date)"
                            ]
                in
                    if hasIssues || True then
                        issues heading
                    else
                        text ""
            )
        |> div []


buttonStyle : List ( String, String )
buttonStyle =
    [ ( "margin-right", "15px" )
    , ( "margin-top", "9px" )
    , ( "border", "0px" )
    , ( "background", "#eee" )
    , ( "box-shadow", "0px 0px 0px 5px rgba(5,5,5,0.2)" )
    , ( "border-radius", "1px" )
    , ( "font-family", "Fira Code, Iosevka, menlo, monospace" )
    ]


listIssues : Maybe (Html Msg) -> List Issue -> Column -> Model -> Column -> String -> Html Msg
listIssues head issues col model addto milestoneNumber =
    let
        lockedIssueNumber =
            model.lockedIssueNumber

        button issue title =
            Html.button
                [ style buttonStyle
                , onClick (IssueAction issue title)
                ]
                [ text title ]

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
                            Just "orange"
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
                        [ span [ cellStyle "400px" ]
                            [ span [ Attrs.class "icon" ] [ text <| getTypeIcon issue ]
                            , Html.a [ Attrs.href issue.htmlUrl, Attrs.target "_blank" ] [ text <| "#" ++ issue.number ]
                            , Html.a
                                [ style [ ( "color", getPriorityColor issue ), ( "cursor", "pointer" ) ]
                                , onClick <| SelectStory issue
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
                                    [ Html.p [] [ Markdown.toHtml [] issue.description ]
                                    , Html.p []
                                        [ text "created by "
                                        , text issue.creator.login
                                        , text " "
                                        , text <| Distance.inWords model.now issue.createdAt
                                        , text " ago"
                                        ]
                                    ]
                              else
                                text ""
                            , div [ Attrs.class "buttons" ] <|
                                case col of
                                    Backlog ->
                                        [ button issue "unplan", button issue "start" ]

                                    Icebox ->
                                        [ button issue "plan"
                                        , if hasLabel "Status: Ready" issue then
                                            button issue "start"
                                          else
                                            text ""
                                        , button issue <|
                                            (if hasLabel "Status: Ready" issue then
                                                "put on ice"
                                             else
                                                "just do it"
                                            )
                                        ]

                                    Current ->
                                        [ button issue "unstart"
                                        , button issue "finish"
                                        ]

                                    Done ->
                                        [ button issue "reopen" ]
                            ]
                        ]
                )
            |> (\list ->
                    case col of
                        Done ->
                            list

                        _ ->
                            (if model.addIssueToColumn == addto && model.addIssueToMilestone == milestoneNumber then
                                Html.form [ cellExStyle [ ( "background", "#333" ) ], Html.Events.onSubmit <| CreateStory col milestone ]
                                    [ Html.input [ style [ ( "width", "90%" ) ], onInput EditNewStoryTitle, Attrs.value model.newIssueTitle ] []
                                    , Html.button [ style buttonStyle ] [ text "Add" ]
                                    , Html.span [ style [ ( "cursor", "pointer" ) ], onClick <| ShowIssueCreationForm Done "" ] [ text "Cancel" ]
                                    ]
                             else
                                Html.span
                                    [ cellExStyle [ ( "text-align", "center" ), ( "background", "#111" ), ( "cursor", "pointer" ), ( "font-weight", "700" ) ]
                                    , onClick <| ShowIssueCreationForm addto milestoneNumber
                                    ]
                                    [ text "add another story" ]
                            )
                                :: list
               )
            |> (\list ->
                    case head of
                        Just htmlNode ->
                            [ Html.span [ cellExStyle [ ( "width", "408px" ), ( "background", "#111" ), ( "padding", "2px" ) ] ] (htmlNode :: list) ]

                        Nothing ->
                            list
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
         , ( "width", "400px" )
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


viewTopbar : User -> Location -> Html msg
viewTopbar user location =
    div []
        [ div
            [ style
                [ ( "position", "absolute" )
                , ( "right", "0px" )
                , ( "vertical-align", "middle" )
                ]
            ]
            [ span [] [ text user.login ]
            , img [ src user.avatar, Attrs.width 24, style [ ( "vertical-align", "middle" ), ( "margin", "5px" ) ] ] []
            ]
        , [ viewLink "stories" (text "Stories")
          , viewLink "milestones" (text "Milestones")
          ]
            |> List.map (\s -> s location)
            |> Html.ul
                [ style
                    [ ( "list-style", "none" )
                    , ( "display", "inline-block" )
                    ]
                ]
        , text "Show stories: "
        , Html.select []
            [ Html.option [] [ text "all" ]
            , Html.option [] [ text "assigned to me" ]
            , Html.option [] [ text "created by me" ]
            , Html.option [] [ text "mentioning me" ]
            ]
        ]


viewLink : String -> Html msg -> Location -> Html msg
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

        color =
            if isActive then
                "rgb(246,246,247)"
            else
                "rgba(255,255,255, 0.1)"

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
        Html.li
            [ style
                [ ( "list-style", "none" )
                , ( "display", "inline-block" )
                , ( "padding", "5px" )
                , ( "margin", "0" )
                , ( "margin-bottom", "10px" )
                , ( "font-weight", "700" )
                , ( "background", color )
                , ( "color", "black" )
                ]
            ]
            [ link ]
