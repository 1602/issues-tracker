module Models exposing (..)

import Base exposing (..)
import Date


-- import Json.Decode as Decode


type Column
    = Current
    | Backlog
    | Icebox
    | Done


type IssueState = IssueOpen | IssueClosed


type alias PersistedData =
    { accessToken : Maybe String
    }


type alias User =
    { login : String
    , avatar : String
    }


type alias Issue =
    { number : Id
    , state : String
    , title : String
    , description : String
    , creator : User
    , assignees : List User
    , milestone : Maybe Milestone
    , htmlUrl : String
    , labels : List Label
    , createdAt : Date.Date
    }

type alias Label =
    { id : Id
    , name : String
    , color : String
    }

type alias Milestone =
    { id : Id
    , number : Id
    , state : String
    , title : String
    , description : Maybe String
    --, creator : User
    , openIssues : Int
    , closedIssues : Int
    , dueOn : Maybe Date.Date
    , htmlUrl : String
    }

type alias ExpandedMilestone =
    { milestone : Milestone
    , openIssues : Maybe (List Issue)
    , closedIssues : Maybe (List Issue)
    }
