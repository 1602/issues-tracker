module Data.Repo exposing (Repo, decoder)

import Json.Decode exposing (Decoder, nullable, string, int, bool)
import Json.Decode.Pipeline exposing (decode, required)


type alias Repo =
    { name : String
    , fullName : String
    , description : Maybe String
    , private : Bool
    , openIssuesCount : Int
    }


decoder : Decoder Repo
decoder =
    decode Repo
        |> required "name" string
        |> required "full_name" string
        |> required "description" (nullable string)
        |> required "private" bool
        |> required "open_issues_count" int
