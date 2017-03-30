module Models exposing (..)

import Base exposing (..)
import Date
import Dict
import Navigation exposing (Location)


-- import Json.Decode as Decode


type Column
    = Current
    | Backlog
    | Icebox
    | Done


type IssueState = IssueOpen | IssueClosed

type alias Model =
    { settings : Settings
    , version : String
    , user : Maybe User
    , token : String
    , accessToken : Maybe String
    , repo : String
    , location : Location
    , now : Date.Date
    , error : Maybe String
    , currentIssues : Maybe (List Issue)
    , iceboxIssues : Maybe (List Issue)
    , closedIssues : Maybe (List Issue)
    , milestones : Maybe (Dict.Dict String ExpandedMilestone)
    , pickMilestoneForIssue : Maybe Issue
    , lockedIssueNumber : String
    , highlightStory : String
    , newMilestoneTitle : String
    , newIssueTitle : String
    , needFocus : Bool
    , addIssueToColumn : Column
    , addIssueToMilestone : String
    , filter : Filter
    , showColumns : List Column
    , pinnedMilestones : Dict.Dict String String
    , filterStoriesBy : String
    , recentRepos : List String
    , etags : Dict.Dict String String
    }


type CachedData
    = CachedData String String String
    | NotCached String
    | NotModified


type alias PersistedData =
    { accessToken : Maybe String
    , pinnedMilestones : List (String, String)
    , columns : List String
    , defaultRepositoryType : String
    , defaultRepository : String
    , recentRepos : List String
    , doneLimit : String
    , powerOfNow : Bool
    }

type alias Settings =
    { defaultRepositoryType : String
    , defaultRepository : String
    , doneLimit : String
    , powerOfNow : Bool
    }

type Filter
    = All
    | CreatedBy String
    | AssignedTo String
    | HasMentionOf String

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
    , updatedAt : Date.Date
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
