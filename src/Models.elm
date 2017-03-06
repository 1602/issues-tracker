module Models exposing (..)

import Base exposing (..)
import Date


-- import Json.Decode as Decode


type Column
    = Current
    | Backlog
    | Icebox
    | Done


type alias PersistedData =
    { user : Maybe AppUser
    }


type alias AppUser =
    { login : String
    , avatar : String
    , secretKey : String
    }


type alias User =
    { login : String
    , avatar : String
    }


type alias Issue =
    { id : Id
    , number : Id
    , state : String
    , title : String
    , description : String
    , assignees : List User
    , milestone : Maybe Milestone
    }

type alias Milestone =
    { id : Id
    , number : Id
    , state : String
    , title : String
    , description : String
    --, creator : User
    , openIssues : Int
    , closedIssues : Int
    , dueOn : Date.Date
    }
