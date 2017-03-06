port module Main exposing (..)

import Route exposing (Route, Route(..), parseHash)
import Models exposing (..)
import Messages exposing (..)
import Services exposing (..)
-- import Base exposing (..)
import Html exposing (Html, span, text, img, div)
-- import Cruft exposing (clipboardIcon)
import Navigation exposing (programWithFlags, Location)
import Http exposing (Error, Response)
-- import Markdown
import Date
-- import Date.Extra
import Time
import Task
-- import Date.Distance as Distance
-- import Json.Decode as Decode exposing (field)
-- import Json.Encode as Encode
-- import List.Extra exposing (find)
import Html.Attributes as Attrs exposing (style, class, attribute, src)
-- import Html.Events exposing (onClick, onInput)


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
    , backlogIssues : Maybe (List Issue)
    , currentIssues : Maybe (List Issue)
    , iceboxIssues : Maybe (List Issue)
    , closedIssues : Maybe (List Issue)
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
                { model | backlogIssues = Just [] }


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
                        , fetchIssues user.secretKey Backlog
                        , fetchIssues user.secretKey Icebox
                        , fetchIssues user.secretKey Done
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


        IssuesLoaded column result ->
            save result
                model
                (\a -> case column of
                    Current ->
                        { model | currentIssues = Just a }
                    Icebox ->
                        { model | iceboxIssues = Just a }
                    Backlog ->
                        { model | backlogIssues = Just a }
                    Done ->
                        { model | closedIssues = Just a }
                )


        CopyText str ->
            model ! [ clipboard str ]


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
                    [ viewTopbar user model.location
                    , viewPage user model <| parseHash model.location
                    , error
                    ]

            Nothing ->
                div [] []


viewPage : AppUser -> Model -> Maybe Route -> Html Msg
viewPage user model route =
    let
        displayIssues issues =
            case issues of
                Just issues ->
                    listIssues issues
                Nothing ->
                    text "Loading..."
    in
        case route of
            Nothing ->
                text "404 not found"

            Just r ->
                case r of
                    IssuesIndex ->
                        Html.main_ [ style
                            [ ("display", "flex")
                            , ("width", "100%")
                            ]
                            ]
                            [ Html.section []
                                [ Html.h1 [] [ text "Icebox" ]
                                , displayIssues model.iceboxIssues
                                ]
                            , Html.section []
                                [ Html.h1 [] [ text "Backlog" ]
                                , displayIssues model.backlogIssues
                                ]
                            , Html.section []
                                [ Html.h1 [] [ text "In progress" ]
                                , displayIssues model.currentIssues
                                ]
                            , Html.section []
                                [ Html.h1 [] [ text "Done" ]
                                , displayIssues model.closedIssues
                                ]
                            ]




listIssues : List Issue -> Html Msg
listIssues issues =
    issues
        |> List.map
            (\issue ->
                div []
                    [ span [ cellStyle "calc(100% - 4px)" ]
                        [ text <| "#" ++ issue.number ++ " " ++ issue.title
                        , text <| case List.head issue.assignees of
                            Just user ->
                                " (on " ++ user.login ++ ")"
                            Nothing ->
                                ""
                        , text <| case issue.milestone of
                            Nothing ->
                                ""
                            Just milestone ->
                                toString milestone.number
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
        -- , ( "white-space", "nowrap" )
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
