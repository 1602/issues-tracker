module Data.Milestone exposing (Milestone, decoder)

import Decoders exposing (intToString, stringToDate)
import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder, field, nullable, maybe)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, optional)
-- import Data.User as User exposing (User)

type alias Milestone =
    { id : String
    , number : String
    , state : String
    , title : String
    , description : Maybe String
    --, creator : User
    , openIssues : Int
    , closedIssues : Int
    , dueOn : Maybe Date.Date
    , htmlUrl : String
    }

decoder : Decoder Milestone
decoder =
    decode Milestone
        |> required "id" intToString
        |> required "number" intToString
        |> required "state" Decode.string
        |> required "title" Decode.string
        |> required "description" (nullable Decode.string)
        -- (field "creator" userDecoder)
        |> required "open_issues" Decode.int
        |> required "closed_issues" Decode.int
        |> required "due_on" (nullable stringToDate)
        |> required "html_url" Decode.string
