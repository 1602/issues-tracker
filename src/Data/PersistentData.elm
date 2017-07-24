module Data.PersistentData exposing (PersistentData, decoder, encode, default)

import Json.Decode as Decode exposing (Decoder, field, nullable, maybe, string, dict, list, bool)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, optional)
import Json.Encode as Encode exposing (Value)
import Dict exposing (Dict)
import Data.Column exposing (Column(..))
import Data.User exposing (User)
import Util exposing ((=>))


type alias PersistentData =
    { accessToken : String
    , savedSearches : Dict String String
    , pinnedMilestones : Dict String String
    , columns : List Column
    , defaultRepositoryType : String
    , defaultRepository : String
    , recentRepos : List String
    , doneLimit : String
    , powerOfNow : Bool
    , version : String
    , user : Maybe User
    }


default : PersistentData
default =
    PersistentData
        ""
        Dict.empty
        Dict.empty
        [ Icebox, Backlog, Current, Done ]
        "specified"
        "universalbasket/engineering"
        []
        "a day"
        False
        "1.0.0"
        Nothing


decoder : Decoder PersistentData
decoder =
    decode PersistentData
        |> optional "accessToken" string ""
        |> optional "savedSearches" (dict string) Dict.empty
        |> optional "pinnedMilestones" (dict string) Dict.empty
        |> optional "columns" (list Data.Column.decoder) [ Icebox, Backlog, Current, Done ]
        |> optional "defaultRepositoryType" string "specified"
        |> optional "defaultRepository" string "universalbasket/engineering"
        |> optional "recentRepos" (list string) []
        |> optional "doneLimit" string "a day"
        |> optional "powerOfNow" bool False
        |> optional "version" string "1.0.0"
        |> optional "user" (nullable Data.User.decoder) Nothing


encode : PersistentData -> Value
encode pd =
    [ "accessToken" => Encode.string pd.accessToken
    , "savedSearches" => encodeDictStrings pd.savedSearches
    , "pinnedMilestones" => encodeDictStrings pd.pinnedMilestones
    , "columns" => Encode.list (List.map Data.Column.encode pd.columns)
    , "defaultRepositoryType" => Encode.string pd.defaultRepositoryType
    , "defaultRepository" => Encode.string pd.defaultRepository
    , "recentRepos" => Encode.list (List.map Encode.string pd.recentRepos)
    , "doneLimit" => Encode.string pd.doneLimit
    , "powerOfNow" => Encode.bool pd.powerOfNow
    , "user" => Encode.null
    ]
        |> Encode.object

encodeDictStrings : Dict String String -> Value
encodeDictStrings dict =
    dict
        |> Dict.toList
        |> List.map (\(s, v) -> (s, Encode.string v))
        |> Encode.object
