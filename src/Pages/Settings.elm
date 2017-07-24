module Pages.Settings exposing (Msg, update, view)

import Data.PersistentData exposing (PersistentData)
import Html exposing (Html, select, text, div)
import Html.Attributes as Attrs exposing (style)
import Html.Events exposing (onClick, onInput)

type Msg
    = ChangeDefaultRepositoryType String
    | UpdateDefaultRepository String
    | ChangeDoneLimit String
    | IgnoreIdeas


update : Msg -> PersistentData -> PersistentData
update msg pd =
    case msg of
        IgnoreIdeas ->
            { pd | powerOfNow = not pd.powerOfNow }

        ChangeDoneLimit s ->
            { pd | doneLimit = s }

        UpdateDefaultRepository s ->
            { pd | defaultRepository = s }

        ChangeDefaultRepositoryType s ->
            { pd | defaultRepositoryType = s }


view : PersistentData -> Html Msg
view persistentData =
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
                [ select ChangeDefaultRepositoryType [ "last visited", "specified" ] persistentData.defaultRepositoryType
                , if persistentData.defaultRepositoryType == "specified" then
                    Html.input [ Attrs.value persistentData.defaultRepository, onInput UpdateDefaultRepository ] []
                  else
                    text ""
                , Html.p [] [ text "this setting controls repository which will be opened when visiting the kanban app" ]
                ]
              -- limit
            , settingsBlock "Limit for 'We just did it'"
                [ select ChangeDoneLimit [ "a day", "a week", "two weeks", "a month" ] persistentData.doneLimit
                , Html.p [] [ text "we only pull fresh issues in 'Done' column, here you can configure what is 'fresh'" ]
                ]
              -- focused mode: ignore milestones with no due date
            , settingsBlock "Focus on present, ignore ideas"
                [ Html.label []
                    [ Html.input [ Attrs.checked persistentData.powerOfNow, Attrs.type_ "checkbox", onClick IgnoreIdeas ] []
                    , text " ignore milestones with no due date"
                    ]
                , Html.p []
                    [ text <|
                        (if persistentData.powerOfNow then
                            "keep this box ticked"
                         else
                            "tick this box"
                        )
                    , text " if you don't want to be bothered by the things that will not happen in the nearest future"
                    ]
                ]
            , settingsBlock "App Version"
                [ Html.strong [] [ text persistentData.version ]
                , Html.p [] [ text "this app is in active development, sometimes you need to refresh app very hard in order to have some old bugs fixed (and possibly grab some new bugs at the same time, sorry), this version number will help you to find whether your cached app version is latest (same as ", Html.a [ Attrs.href "https://github.com/1602/issues-tracker/blob/master/package.json#L4" ] [ text "here" ], text ")." ]
                ]
            ]

