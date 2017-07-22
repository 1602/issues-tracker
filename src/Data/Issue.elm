module Data.Issue exposing (Issue, decoder)

import Decoders exposing (intToString, stringToDate)
import Json.Decode as Decode exposing (Decoder, field, nullable, maybe)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, optional)
import Date exposing (Date)
import Data.User as User exposing (User)
import Data.Label as Label exposing (Label)
import Data.Milestone as Milestone exposing (Milestone)

type alias Issue =
    { number : String
    , state : String
    , title : String
    , description : String
    , creator : User
    , assignees : List User
    , milestone : Maybe Milestone
    , htmlUrl : String
    , labels : List Label
    , createdAt : Date.Date
    , updatedAt : Date.Date
    }

decoder : Decoder Issue
decoder =
    decode Issue
        -- (field "id" decodeIntToString)
        |> required "number" intToString
        |> required "state" Decode.string
        |> required "title" Decode.string
        |> required "body" Decode.string
        |> required "user" User.decoder
        |> required "assignees" (Decode.list User.decoder)
        |> required "milestone" (nullable Milestone.decoder)
        |> required "html_url" Decode.string
        |> required "labels" (Decode.list Label.decoder)
        |> required "created_at" stringToDate
        |> required "updated_at" stringToDate

