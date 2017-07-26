module Decoders exposing (intToDate, intToString, stringToDate)

import Json.Decode as Decode
import Date


intToDate : Decode.Decoder Date.Date
intToDate =
    Decode.int
        |> Decode.andThen
            (\val ->
                Decode.succeed <| Date.fromTime <| toFloat val
            )


intToString : Decode.Decoder String
intToString =
    Decode.int
        |> Decode.andThen
            (\val ->
                Decode.succeed <| toString val
            )


stringToDate : Decode.Decoder Date.Date
stringToDate =
    Decode.string
        |> Decode.andThen
            (\val ->
                case Date.fromString val of
                    Err err ->
                        Decode.fail err

                    Ok date ->
                        Decode.succeed date
            )
