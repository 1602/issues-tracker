module Pages.Repos exposing (Msg, Model, view, init, update)

import Http exposing (Error)
import Html exposing (div, text, ul, li)
import Html.Attributes exposing (href)
import Data.Repo as Repo exposing (Repo)
import Request.Repo
import Data.PersistentData exposing (PersistentData)
import Request.Cache exposing (Etags, RemoteData, retrieveError, retrieveData, updateCache)
import Dict

type Msg
    = LoadedReposList (Result Error (RemoteData (List Repo)))

type alias Model =
    { list : List Repo
    , accessToken : String
    , error : Maybe String
    , cache : Etags
    }

init : PersistentData -> (Model, Cmd Msg)
init pd =
    let
        model =
            Model
                []
                pd.accessToken
                Nothing
                Dict.empty
    in
       model ! [ Request.Repo.list model.accessToken model.cache |> Http.send LoadedReposList ]


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
        LoadedReposList result ->
            { model
                | list = retrieveData result model.list
                , error = retrieveError result
                , cache = updateCache result model.cache
            }
