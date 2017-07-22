module Models exposing (..)

import Date
import Dict
import Navigation exposing (Location)
import Data.Issue exposing (Issue)
import Data.Milestone exposing (Milestone)
import Data.User exposing (User)


-- import Json.Decode as Decode


type Column
    = Current
    | Backlog
    | Icebox
    | Done
    | Search


type IssueState = OpenIssue | ClosedIssue

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
    , milestones : Dict.Dict String (Dict.Dict String ExpandedMilestone)
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
    , etags : Dict.Dict String (String, String)
    , searchTerms : String
    , searchResults : RemoteData (List Issue)
    , didSwitch : Bool
    , savedSearches : Dict.Dict String String
    }

type RemoteData a
    = NotRequested
    | Loading
    | Loaded a


type CachedData
    = CachedData String String String
    | NotCached String


type alias PersistedData =
    { accessToken : Maybe String
    , savedSearches : List (String, String)
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


type alias ExpandedMilestone =
    { milestone : Milestone
    , openIssues : Maybe (List Issue)
    , closedIssues : Maybe (List Issue)
    }
