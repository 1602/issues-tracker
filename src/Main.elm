module Main exposing (..)

import Route exposing (Route, Route(..), parseHash)
import Html exposing (Html, span, text, img, div)
import Navigation exposing (programWithFlags, Location)
import Http exposing (Error(..), Response)
import Html.Attributes as Attrs exposing (style, class, attribute, src)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Data.Issue as Issue exposing (Issue)
import Data.User as User exposing (User)
import Request.User
import Data.PersistentData exposing (PersistentData)
import Json.Decode exposing (Value)
import Ports exposing (saveData)
import Pages.Repos
import Pages.Roadmap
import Pages.Board
import Pages.Setup


main : Program Value Model Msg
main =
    programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL

type alias Model =
    { version : String
    , user : Maybe User
    , repo : (String, String)
    , token : String
    , location : Location
    , persistentData : PersistentData
    , repos : Pages.Repos.Model
    , roadmap : Pages.Roadmap.Model
    , board : Pages.Board.Model
    , setup : Pages.Setup.Model
    }


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

        repo =
            case page of
                Just (Stories user repo) ->
                    (user, repo)
                Just (Story user repo _) ->
                    (user, repo)
                Just (Settings user repo) ->
                    (user, repo)
                Just (Milestones user repo) ->
                    (user, repo)
                _ ->
                    ("", "")

        persistentData =
            initialData
                |> Decode.decodeValue Data.PersistentData.decoder
                |> Result.toMaybe
                |> Maybe.withDefault Data.PersistentData.default

        (repos, cmdRepos) =
            Pages.Repos.init persistentData

        (roadmap, cmdRoadmap) =
            Pages.Roadmap.init persistentData repo

        (board, cmdBoard) =
            Pages.Board.init persistentData location

        setup =
            Pages.Setup.init

        model =
            Model
                -- version
                "1.1.1"
                -- user
                Nothing
                -- repo
                repo
                -- token
                ""
                -- location
                location
                -- persistentData
                persistentData
                -- repos
                repos
                -- roadmap
                roadmap
                -- board
                board
                -- setup
                setup

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
        model !
            [
            if persistentData.accessToken == "" then
                Cmd.none
            else
                Request.User.get persistentData.accessToken |> Http.send LoadUser
            , Cmd.map ReposMsgProxy cmdRepos
            , Cmd.map RoadmapMsgProxy cmdRoadmap
            , Cmd.map BoardMsgProxy cmdBoard
            ]


hasLabel : String -> Issue -> Bool
hasLabel label issue =
    issue.labels
        |> List.map .name
        |> List.member label


hasNoLabel : String -> Issue -> Bool
hasNoLabel label issue =
    not <| hasLabel label issue


updateLocalStorage : Model -> Cmd msg
updateLocalStorage model =
    model.persistentData
        |> Data.PersistentData.encode
        |> saveData



-- UPDATE


type Msg
    = SettingsMsgProxy SettingsMsg
    | RoadmapMsgProxy Pages.Roadmap.Msg
    | ReposMsgProxy Pages.Repos.Msg
    | SetupMsgProxy Pages.Setup.Msg
    | BoardMsgProxy Pages.Board.Msg
    | UrlChange Location
    | LoadUser (Result Error User)


type SettingsMsg
    = ChangeDefaultRepositoryType String
    | UpdateDefaultRepository String
    | ChangeDoneLimit String
    | IgnoreIdeas


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
        SettingsMsgProxy msg ->
            let
                updatedModel =
                    { model | persistentData = updateSettings msg model.persistentData }
            in
                updatedModel ! [ updateLocalStorage updatedModel ]

        ReposMsgProxy msg ->
            { model | repos = Pages.Repos.update msg model.repos } ! []

        BoardMsgProxy msg ->
            let
                (board, cmd) =
                    Pages.Board.update msg model.board model.persistentData
            in
                { model | board = board } ! [ Cmd.map BoardMsgProxy cmd ]

        SetupMsgProxy msg ->
            let
                (setup, cmd) =
                    Pages.Setup.update msg model.setup
            in
                { model | setup = setup } ! [ Cmd.map SetupMsgProxy cmd ]

        RoadmapMsgProxy msg ->
            let
                (roadmap, cmd) =
                    Pages.Roadmap.update msg model.roadmap
            in
                { model | roadmap = roadmap } ! [ Cmd.map RoadmapMsgProxy cmd ]

        UrlChange location ->
            { model | location = location } ! []


        LoadUser user ->
            case user of
                Ok user ->
                    { model | user = Just user } ! []

                Err _ ->
                    model ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Pages.Board.subscriptions model.board |> Sub.map BoardMsgProxy
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ style [ ( "display", "flex" ) ] ] <|
        [ viewNavigation model.user model
        , if model.persistentData.accessToken /= "" then
            viewPage model.user model <| parseHash model.location
          else
              Html.map SetupMsgProxy Pages.Setup.view
        ]


viewPage : Maybe User -> Model -> Maybe Route -> Html Msg
viewPage user model route =
    case route of
        Nothing ->
            Html.map ReposMsgProxy <| Pages.Repos.view model.repos

        Just r ->
            case r of
                Repos ->
                    Html.map ReposMsgProxy <| Pages.Repos.view model.repos

                Story user repo id ->
                    Html.map BoardMsgProxy <| Pages.Board.view model.board model.persistentData

                Stories user repo ->
                    Html.map BoardMsgProxy <| Pages.Board.view model.board model.persistentData

                Milestones user repo ->
                    Html.map RoadmapMsgProxy <| Pages.Roadmap.view model.roadmap

                Settings user repo ->
                    Html.map SettingsMsgProxy <| viewSettings model


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


viewNavigation : Maybe User -> Model -> Html Msg
viewNavigation user model =
    let
        (u, r) =
            model.repo

        showColumns =
            model.persistentData.columns


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
                    Pages.Board.issuesSubnav model.persistentData |> Html.map BoardMsgProxy

                Just (Stories _ _) ->
                    Pages.Board.issuesSubnav model.persistentData |> Html.map BoardMsgProxy

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
                                [ Route.href <| Settings u r
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
            , subNav
            ]


