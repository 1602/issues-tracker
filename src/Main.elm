module Main exposing (main)

import Route exposing (Route(..), parseHash)
import Html exposing (Html, text, img, div)
import Navigation exposing (programWithFlags, Location)
import Http exposing (Error)
import Html.Attributes as Attrs exposing (style, src)
import Data.User exposing (User)
import Request.User
import Data.PersistentData exposing (PersistentData)
import Json.Decode exposing (Value, decodeValue, decodeValue)
import Ports exposing (saveData)
import Pages.Repos
import Pages.Roadmap
import Pages.Board
import Pages.Setup
import Pages.Settings


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
    , repo : ( String, String )
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

        repo =
            case page of
                Just (Stories u r) ->
                    ( u, r )

                Just (Story u r _) ->
                    ( u, r )

                Just (Settings u r) ->
                    ( u, r )

                Just (Milestones u r) ->
                    ( u, r )

                _ ->
                    ( "", "" )

        persistentData =
            initialData
                |> decodeValue Data.PersistentData.decoder
                |> Result.toMaybe
                |> Maybe.withDefault Data.PersistentData.default

        ( repos, cmdRepos ) =
            Pages.Repos.init persistentData

        ( roadmap, cmdRoadmap ) =
            Pages.Roadmap.init persistentData repo

        ( board, cmdBoard ) =
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

    in
        model
            ! [ if persistentData.accessToken == "" then
                    Cmd.none
                else
                    Request.User.get persistentData.accessToken |> Http.send LoadUser
              , Cmd.map ReposMsgProxy cmdRepos
              , Cmd.map RoadmapMsgProxy cmdRoadmap
              , Cmd.map BoardMsgProxy cmdBoard
              ]



updateLocalStorage : PersistentData -> Cmd msg
updateLocalStorage persistentData =
    persistentData
        |> Data.PersistentData.encode
        |> saveData



-- UPDATE


type Msg
    = SettingsMsgProxy Pages.Settings.Msg
    | RoadmapMsgProxy Pages.Roadmap.Msg
    | ReposMsgProxy Pages.Repos.Msg
    | SetupMsgProxy Pages.Setup.Msg
    | BoardMsgProxy Pages.Board.Msg
    | UrlChange Location
    | LoadUser (Result Error User)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- TODO: separate actions which require user
    case msg of
        SettingsMsgProxy m ->
            let
                upd =
                    Pages.Settings.update m model.persistentData
            in
                model ! [ updateLocalStorage upd ]

        ReposMsgProxy m ->
            { model | repos = Pages.Repos.update m model.repos } ! []

        BoardMsgProxy m ->
            let
                ( board, cmd ) =
                    Pages.Board.update m model.board model.persistentData
            in
                { model | board = board } ! [ Cmd.map BoardMsgProxy cmd ]

        SetupMsgProxy m ->
            let
                ( setup, cmd ) =
                    Pages.Setup.update m model.setup
            in
                { model | setup = setup } ! [ Cmd.map SetupMsgProxy cmd ]

        RoadmapMsgProxy m ->
            let
                ( roadmap, cmd ) =
                    Pages.Roadmap.update m model.roadmap
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

                Story _ _ _ ->
                    Html.map BoardMsgProxy <| Pages.Board.view model.board model.persistentData

                Stories _ _ ->
                    Html.map BoardMsgProxy <| Pages.Board.view model.board model.persistentData

                Milestones _ _ ->
                    Html.map RoadmapMsgProxy <| Pages.Roadmap.view model.roadmap

                Settings _ _ ->
                    Html.map SettingsMsgProxy <| Pages.Settings.view model.persistentData


viewNavigation : Maybe User -> Model -> Html Msg
viewNavigation user model =
    let
        ( u, r ) =
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

        viewLink name childNode location =
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
                        |> String.startsWith name

                url =
                    "#/" ++ repo ++ "/" ++ name

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
