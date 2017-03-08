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


-- import Base exposing (..)
-- import Cruft exposing (clipboardIcon)
-- import Date.Extra
-- import Markdown
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



-- MODEL


type alias Model =
    { user : Maybe AppUser
    , location : Location
    , now : Date.Date
    , error : Maybe String
    , currentIssues : Maybe (List Issue)
    , iceboxIssues : Maybe (List Issue)
    , closedIssues : Maybe (List Issue)
    , milestones : Maybe (Dict.Dict String ExpandedMilestone)
    , pickMilestoneForIssue : Maybe Issue
    }


init : PersistedData -> Location -> ( Model, Cmd Msg )
init persistentData location =
    Model
        persistentData.user
        location
        (Date.fromTime <| Time.millisecond * (toFloat 0))
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        ! ([ Task.perform CurrentDate Date.now
           , case persistentData.user of
                Just user ->
                    Cmd.none

                --fetchClients user.secretKey
                Nothing ->
                    Cmd.none
           ]
            ++ (loadResource location persistentData.user)
          )


aboutToLoadResource : Location -> Model -> Model
aboutToLoadResource loc model =
    let
        page =
            parseHash loc
    in
        case page of
            Just IssuesIndex ->
                { model | iceboxIssues = Just [] }

            Nothing ->
                model


loadResource : Location -> Maybe AppUser -> List (Cmd Msg)
loadResource loc user =
    let
        page =
            parseHash loc
    in
        case user of
            Just user ->
                case page of
                    Just IssuesIndex ->
                        [ fetchIssues user.secretKey Current
                        , fetchIssues user.secretKey Icebox
                        , fetchMilestones user.secretKey
                        ]

                    Nothing ->
                        []

            Nothing ->
                []


port googleAuth : (String -> msg) -> Sub msg


port saveData : PersistedData -> Cmd msg


port clipboard : String -> Cmd msg



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        CurrentDate now ->
            { model | now = now } ! []

        CurrentTime now ->
            { model | now = Date.fromTime now } ! []

        UrlChange location ->
            ({ model | location = location }
                |> aboutToLoadResource location
            )
                ! loadResource location model.user

        MilestoneIssuesLoaded num issueState result ->
            case result of
                Err error ->
                    { model | error = Just (toString error) } ! []

                Ok issues ->
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
                                                            Just { ms | openIssues = Just issues }

                                                        IssueClosed ->
                                                            Just { ms | closedIssues = Just issues }

                                                Nothing ->
                                                    Nothing
                                        )
                                )
                    }
                        ! []

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
                                        Dict.insert ms.number <| ExpandedMilestone ms Nothing Nothing
                                    )
                                    Dict.empty
                                |> Just
                        , error = Nothing
                    }
                        ! (case model.user of
                            Just user ->
                                (milestones |> List.map (fetchMilestoneIssues user.secretKey IssueOpen))
                                    ++ (milestones |> List.map (fetchMilestoneIssues user.secretKey IssueClosed))

                            Nothing ->
                                []
                          )

        IssuesLoaded column result ->
            save result
                model
                (\a ->
                    case column of
                        Current ->
                            { model | currentIssues = Just a }

                        Icebox ->
                            { model | iceboxIssues = Just a }

                        Done ->
                            { model | closedIssues = Just a }

                        Backlog ->
                            model
                )

        CopyText str ->
            model ! [ clipboard str ]

        UnsetMilestone m result ->
            model !
            (case model.user of
                Just user ->
                    [ fetchMilestoneIssues user.secretKey IssueOpen m
                    , fetchIssues user.secretKey Icebox
                    ]

                Nothing ->
                    []
                    )

        SetMilestone issue milestone ->
            case model.user of
                Just user ->
                    { model | pickMilestoneForIssue = Nothing } !
                        [ updateIssueWith issue.number (Encode.object
                            [ ("milestone"
                                , milestone.number
                                |> String.toInt
                                |> Result.toMaybe
                                |> Maybe.withDefault 0
                                |> Encode.int
                                )
                            ]
                            ) user.secretKey (MilestoneSet milestone)
                        ]

                Nothing ->
                    model ! []

        MilestoneSet m result ->
            model !
            (case model.user of
                Just user ->
                    [ fetchMilestoneIssues user.secretKey IssueOpen m
                    , fetchIssues user.secretKey Icebox
                    ]

                Nothing ->
                    []
                    )

        IssueAction issue action ->
            case model.user of
                Just user ->
                    case action of
                        "unplan" ->
                            case issue.milestone of
                                Just m ->
                                    model !
                                        [ updateIssue issue user.secretKey (UnsetMilestone m)
                                        ]

                                Nothing ->
                                    model ! []
                        "plan" ->
                            { model | pickMilestoneForIssue = Just issue } ! []

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
        [ Time.every (30 * Time.second) CurrentTime
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
                    -- [ viewTopbar user model.location
                    [ viewPage user model <| parseHash model.location
                    , error
                    , case model.pickMilestoneForIssue of
                        Just issue ->
                            div [ style
                                [ ( "position", "fixed" )
                                , ( "top", "10px" )
                                , ( "left", "10px" )
                                , ( "padding", "10px" )
                                , ( "background", "black" )
                                ]
                                ] [
                                    model.milestones
                                        |> Maybe.withDefault Dict.empty
                                        |> Dict.values
                                        |> List.map (\s ->
                                            Html.li [] [ Html.button [ onClick <| SetMilestone issue s.milestone ] [ text s.milestone.title ] ]
                                            )
                                        |> Html.ul []
                                ]

                        Nothing ->
                            text ""
                    ]

            Nothing ->
                div [] []


viewPage : AppUser -> Model -> Maybe Route -> Html Msg
viewPage user model route =
    let
        displayIssuesWithinMilestones milestones issueState =
            case milestones of
                Just milestones ->
                    listIssuesWithinMilestones milestones issueState model.now

                Nothing ->
                    span [ cellStyle "400px" ] [ text "Loading..." ]

        displayIssues issues col =
            case issues of
                Just issues ->
                    listIssues issues col

                Nothing ->
                    span [ cellStyle "400px" ] [ text "Loading..." ]
    in
        case route of
            Nothing ->
                text "404 not found"

            Just r ->
                case r of
                    IssuesIndex ->
                        Html.main_
                            [ style
                                [ ( "display", "flex" )
                                , ( "width", "100%" )
                                ]
                            ]
                            [ Html.section []
                                [ Html.h3 [] [ text "❄ Icebox" ]
                                , displayIssues model.iceboxIssues Icebox
                                ]
                            , Html.section []
                                [ Html.h3 [] [ text "🚥 Backlog" ]
                                , displayIssuesWithinMilestones model.milestones IssueOpen
                                ]
                            , Html.section []
                                [ Html.h3 [] [ text "🐝 In progress" ]
                                , displayIssues model.currentIssues Current
                                ]
                            , Html.section []
                                [ Html.h3 [] [ text "🎉 Done" ]
                                , displayIssuesWithinMilestones model.milestones IssueClosed
                                ]
                            ]


listIssuesWithinMilestones : Dict.Dict String ExpandedMilestone -> IssueState -> Date.Date -> Html Msg
listIssuesWithinMilestones milestones issueState now =
    milestones
        |> Dict.values
        |> List.sortBy (\ems -> case ems.milestone.dueOn of
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
                    filterOutInProgress issues =
                        issues
                            |> List.filter (\issue ->
                                issue.labels
                                    |> List.any (\label -> label.name == "Status: In Progress")
                                    |> not
                                    )

                    displayIssuesOrLoading issues =
                        case issues of
                            Just issues ->
                                listIssues (filterOutInProgress issues)
                                    (case issueState of
                                        IssueOpen ->
                                            Backlog

                                        IssueClosed ->
                                            Done
                                    )

                            Nothing ->
                                span [ cellStyle "400px" ] [ text "Loading" ]

                    issues =
                        case issueState of
                            IssueOpen ->
                                displayIssuesOrLoading expandedMilestone.openIssues

                            IssueClosed ->
                                displayIssuesOrLoading expandedMilestone.closedIssues
                in
                    div []
                        [ span [ cellStyle "400px" ]
                            [ Html.strong [ style [ ( "color", "yellowgreen" ) ] ]
                                [ text <| "🏁 " ++ expandedMilestone.milestone.title ]
                            , case expandedMilestone.milestone.dueOn of
                                Just date ->
                                    if (Date.toTime date |> Time.inSeconds) < (Date.toTime now |> Time.inSeconds) then
                                        text " overdue"
                                    else
                                        text <| " due in " ++ (Distance.inWords now date)

                                Nothing ->
                                    text " (no due date)"
                            ]
                        , issues
                        ]
            )
        |> div []


listIssues : List Issue -> Column -> Html Msg
listIssues issues col =
    let
        filterOutInProgress issues =
            case col of
                Icebox ->
                    issues
                        |> List.filter (\issue ->
                            issue.labels
                                |> List.any (\label -> label.name == "Status: In Progress")
                                |> not
                                )

                _ ->
                    issues

        button issue title =
            Html.button
                [ style
                    [ ( "margin-right", "15px" )
                    , ( "margin-top", "9px" )
                    , ( "border", "0px" )
                    , ( "background", "#eee" )
                    , ( "box-shadow", "0px 0px 0px 5px rgba(5,5,5,0.2)" )
                    , ( "border-radius", "1px" )
                    , ( "font-family", "Fira Code, Iosevka, menlo, monospace" )
                    ]
                ,   onClick (IssueAction issue title)
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
                |> Maybe.withDefault "❓ "

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
    in
        issues
            |> filterOutInProgress
            |> List.map
                (\issue ->
                    div [ Attrs.class "story" ]
                        [ span [ cellStyle "400px" ]
                            [ text <| getTypeIcon issue
                            , Html.a [ Attrs.href issue.htmlUrl, Attrs.target "_blank" ] [ text <| "#" ++ issue.number ]
                            , span [ style [ ( "color", getPriorityColor issue ) ] ] [ text <| " " ++ issue.title ++ " " ]
                            , Html.i [ style [ ( "color", "darkgrey" ) ] ]
                                [ text <|
                                    if List.length issue.assignees == 0 then
                                        "(unassigned)"
                                    else
                                        "(on " ++ (issue.assignees |> List.map .login |> String.join ", ") ++ ")"
                                ]
                            , div [ Attrs.class "buttons" ] <|
                                case col of
                                    Backlog ->
                                        [ button issue "unplan", button issue "start" ]

                                    Icebox ->
                                        [ button issue "plan" ]

                                    Current ->
                                        [ button issue "unstart", button issue "finish" ]

                                    Done ->
                                        [ button issue "reopen" ]
                            ]
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
        , ( "white-space", "nowrap" )
        , ( "background", "rgba(255,255,255,0.1)" )
        , ( "position", "relative" )
        , ( "box-sizing", "border-box" )
        ]


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


viewTopbar : AppUser -> Location -> Html msg
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
        , [ viewLink "issues" (text "Issues")
          ]
            |> List.map (\s -> s location)
            |> Html.ul
                [ style
                    [ ( "list-style", "none" )
                    , ( "display", "inline-block" )
                    ]
                ]
        ]


viewLink : String -> Html msg -> Location -> Html msg
viewLink src childNode location =
    let
        isActive =
            String.startsWith url location.hash

        color =
            if isActive then
                "#ff9b89"
            else
                "rgba(255,255,255, 0.1)"

        url =
            "#/" ++ src

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
                , ( "margin", "5px" )
                , ( "font-weight", "700" )
                , ( "background", color )
                , ( "color", "black" )
                ]
            ]
            [ link ]
