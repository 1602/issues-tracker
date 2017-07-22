port module Ports exposing (..)

import Json.Decode exposing (Value)


port saveData : Value -> Cmd msg


port clipboard : String -> Cmd msg
