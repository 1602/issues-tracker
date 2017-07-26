module Data.User exposing (User, decoder)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)


type alias User =
    { login : String
    , avatar : String
    }


decoder : Decode.Decoder User
decoder =
    decode User
        |> required "login" Decode.string
        |> required "avatar_url" Decode.string
