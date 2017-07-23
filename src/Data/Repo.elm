module Data.Repo exposing (Repo, decoder)

import Json.Decode as Decode exposing (Decoder, field, nullable, maybe, string, int, bool)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, optional)

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
