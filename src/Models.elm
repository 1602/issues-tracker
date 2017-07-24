module Models exposing (..)

import Data.Milestone exposing (Milestone)
import Data.User exposing (User)
import Data.PersistentData exposing (PersistentData)
import Pages.Repos
import Pages.Roadmap
import Pages.Board
import Navigation exposing (Location)




type RemoteData a
    = NotRequested
    | Loading
    | Loaded a


