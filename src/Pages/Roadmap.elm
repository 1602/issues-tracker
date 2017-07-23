module Pages.Roadmap exposing (Model, Msg, init, view, update)

import Html exposing (Html, div, text)
import Dict exposing (Dict)
import Data.Milestone as Milestone exposing (Milestone)
import Request.Milestone

type Msg =
    None

type alias Model =
    { list : List Milestone
    }


init : String -> (String, String) -> (Model, Cmd Msg)
init { accessToken } =
    Model [] ! [ Request.Milestone.list accessToken ]


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    model ! []


view : Model -> Html Msg
view model =
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
