module Data.Label exposing (Label, decoder)

import Decoders exposing (intToString)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)


decoder : Decode.Decoder Label
decoder =
    decode Label
        |> required "id" intToString
        |> required "name" Decode.string
        |> required "color" Decode.string


type alias Label =
    { id : String
    , name : String
    , color : String
    }
