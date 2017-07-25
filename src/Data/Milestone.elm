module Data.Milestone exposing (Milestone, decoder)

import Decoders exposing (intToString, stringToDate)
import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder, nullable)
import Json.Decode.Pipeline exposing (decode, required)


-- import Data.User as User exposing (User)


type alias Milestone =
    { id : String
    , number : String
    , state : String
    , title : String
    , description :
        Maybe String
    , openIssues : Int
    , closedIssues : Int
    , dueOn : Maybe Date
    , htmlUrl :
        String
        --, creator : User
    }


decoder : Decoder Milestone
decoder =
    decode Milestone
        |> required "id" intToString
        |> required "number" intToString
        |> required "state" Decode.string
        |> required "title" Decode.string
        |> required "description" (nullable Decode.string)
        |> required "open_issues" Decode.int
        |> required "closed_issues" Decode.int
        |> required "due_on" (nullable stringToDate)
        |> required "html_url" Decode.string



-- (field "creator" userDecoder)
