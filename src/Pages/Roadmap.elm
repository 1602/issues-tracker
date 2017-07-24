module Pages.Roadmap exposing (Model, Msg, init, view, update)

import Dict
import Html exposing (Html, div, text)
import Html.Attributes as Attrs exposing (href, style)
import Data.Milestone as Milestone exposing (Milestone)
import Request.Milestone
import Http
import Request.Cache exposing (Etags, CachedResult, retrieveData, updateCache, retrieveError)
import Date exposing (Date)
import Time exposing (Time)
import Date.Distance as Distance

type Msg =
    LoadMilestones (CachedResult (List Milestone))

type alias Model =
    { list : List Milestone
    , accessToken : String
    , repo : (String, String)
    , cache : Etags
    , now : Date
    , error : Maybe String
    }


init : String -> (String, String) -> (Model, Cmd Msg)
init accessToken repo =
    let
        model =
            Model
                []
                accessToken
                repo
                -- cache
                Dict.empty
                -- now
                (Date.fromTime <| Time.millisecond * (toFloat 0))
                -- error
                Nothing
    in
       model ! [ Request.Milestone.list model.repo model.accessToken model.cache |> Http.send LoadMilestones ]


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        LoadMilestones result ->
            { model
                | list = retrieveData result model.list
                , error = retrieveError result
                , cache = updateCache result model.cache
                } ! []


view : Model -> Html Msg
view model =
    let
        milestones =
            model.list
    in
        milestones
            |> List.sortBy
                (\milestone ->
                    case milestone.dueOn of
                        Just date ->
                            Date.toTime date |> Time.inHours

                        Nothing ->
                            1 / 0
                )
            |> List.map
                (\milestone ->
                    let
                        isOverdue =
                            case milestone.dueOn of
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
                                  , case milestone.dueOn of
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
                            [ Html.a [ Attrs.target "_blank", href milestone.htmlUrl ] [ text <| milestone.title ++ " " ]
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
                                    case milestone.dueOn of
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

