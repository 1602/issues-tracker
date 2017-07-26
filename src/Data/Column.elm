module Data.Column exposing (Column(Current, Backlog, Icebox, Done, Search), decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Column
    = Current
    | Backlog
    | Icebox
    | Done
    | Search


decoder : Decoder Column
decoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Current" ->
                        Decode.succeed Current

                    "Done" ->
                        Decode.succeed Done

                    "Icebox" ->
                        Decode.succeed Icebox

                    "Search" ->
                        Decode.succeed Search

                    "Backlog" ->
                        Decode.succeed Backlog

                    somethingElse ->
                        Decode.fail <| "Unknown column type: " ++ somethingElse
            )


encode : Column -> Value
encode column =
    case column of
        Current ->
            Encode.string "Current"

        Done ->
            Encode.string "Done"

        Icebox ->
            Encode.string "Icebox"

        Search ->
            Encode.string "Search"

        Backlog ->
            Encode.string "Backlog"
