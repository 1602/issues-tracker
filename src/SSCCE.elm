module SSCCE exposing (main)

import Html exposing (program, Html, text, li, ul)
import Html.Attributes exposing (href)
import Http exposing (Error)
import Data.Repo exposing (Repo)
import Request.Repo
import Time exposing (Time)
import Request.Cache exposing (Etags, RemoteData, retrieveError, retrieveData, updateCache)
import Dict


type Msg
    = LoadedReposList (Result Error (RemoteData (List Repo)))
    | Poll Time


type alias Model =
    { list : List Repo
    , accessToken : String
    , error : Maybe String
    , cache : Etags
    }


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    let
        token =
            "your github token with `repo` scope access"

        model =
            Model
                []
                token
                Nothing
                Dict.empty
    in
        model ! [ Request.Repo.list token model.cache |> Http.send LoadedReposList ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Poll _ ->
            model ! [ Request.Repo.list model.accessToken model.cache |> Http.send LoadedReposList ]

        LoadedReposList result ->
            { model
                | list = retrieveData result model.list
                , error = retrieveError result
                , cache = updateCache result model.cache
            }
                ! []


view : Model -> Html Msg
view model =
    model.list
        |> List.map
            (\x ->
                li []
                    [ Html.a
                        [ href <| "#/" ++ x.fullName ++ "/stories" ]
                        [ text x.fullName
                        ]
                    , text <| " (" ++ toString x.openIssuesCount ++ " open issues)"
                    ]
            )
        |> ul []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every (1 * Time.second) Poll
        ]
