module Pages.Repos exposing (Msg, Model, view, init, update)

import Http
import Html exposing (div, text, ul, li)
import Html.Attributes exposing (href)
import Data.Repo as Repo exposing (Repo)
import Request.Repo
import Data.PersistentData exposing (PersistentData)

type Msg
    = LoadedReposList (Result Http.Error (List Repo))

type alias Model =
    { list : List Repo
    , accessToken : String
    , error : Maybe Http.Error
    }

init : PersistentData -> (Model, Cmd Msg)
init pd =
    Model [] pd.accessToken Nothing ! [
        Request.Repo.list pd.accessToken |> Http.send LoadedReposList
    ]


view : Model -> Html.Html Msg
view model =
    model.list
        |> List.map (\x -> li []
            [ Html.a
                [ href <| "#/" ++ x.fullName ++ "/stories" ]
                [ text x.fullName
                ]
            , text <| " (" ++ (toString x.openIssuesCount) ++ " open issues)"
            ] )
        |> ul []


update : Msg -> Model -> Model
update msg model =
    case msg of
        LoadedReposList r ->
            case r of
                Ok repos ->
                    { model | list = repos, error = Nothing }
                Err e ->
                    { model | list = [], error = Just e }
