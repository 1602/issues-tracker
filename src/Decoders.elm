module Decoders exposing (..)

import Json.Decode as Decode exposing (field, nullable, maybe)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, optional)
import Date
import Models exposing (..)


decodeIntToDate : Decode.Decoder Date.Date
decodeIntToDate =
    Decode.int
        |> Decode.andThen
            (\val ->
                Decode.succeed <| Date.fromTime <| toFloat val
            )

decodeStringToDate : Decode.Decoder Date.Date
decodeStringToDate =
    Decode.string
        |> Decode.andThen
            (\val ->
                case Date.fromString val of
                    Err err ->
                        Decode.fail err
                    Ok date ->
                        Decode.succeed date
            )


decodeIntToString : Decode.Decoder String
decodeIntToString =
    Decode.int
        |> Decode.andThen
            (\val ->
                Decode.succeed <| toString val
            )

issueDecoder : Decode.Decoder Issue
issueDecoder =
    Decode.map8 Issue
        -- (field "id" decodeIntToString)
        (field "number" decodeIntToString)
        (field "state" Decode.string)
        (field "title" Decode.string)
        (field "body" Decode.string)
        (field "assignees" <| Decode.list userDecoder)
        (maybe <| field "milestone" milestoneDecoder)
        (field "html_url" Decode.string)
        (field "labels" <| Decode.list labelDecoder)

labelDecoder : Decode.Decoder Label
labelDecoder =
    Decode.map3 Label
        (field "id" decodeIntToString)
        (field "name" Decode.string)
        (field "color" Decode.string)

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (field "login" Decode.string)
        (field "avatar_url" Decode.string)


milestoneDecoder : Decode.Decoder Milestone
milestoneDecoder =
    decode Milestone
        |> required "id" decodeIntToString
        |> required "number" decodeIntToString
        |> required "state" Decode.string
        |> required "title" Decode.string
        |> required "description" (nullable Decode.string)
        -- (field "creator" userDecoder)
        |> required "open_issues" Decode.int
        |> required "closed_issues" Decode.int
        |> required "due_on" (nullable decodeStringToDate)
        |> required "html_url" Decode.string
