module Data.User exposing (User, decoder)

import Json.Decode as Decode exposing (field, nullable, maybe)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, optional)

type alias User =
    { login : String
    , avatar : String
    }


decoder : Decode.Decoder User
decoder =
    decode User
        |> required "login" Decode.string
        |> required "avatar_url" Decode.string

