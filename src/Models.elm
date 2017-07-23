module Models exposing (..)

import Date
import Dict
import Navigation exposing (Location)
import Data.Issue exposing (Issue)
import Data.Milestone exposing (Milestone)
import Data.User exposing (User)
import Data.Column exposing (Column(..))
import Data.PersistentData exposing (PersistentData)
import Pages.Repos


type IssueState = OpenIssue | ClosedIssue


type alias Model =
    { version : String
    , user : Maybe User
    , token : String
    , repo : (String, String)
    , location : Location
    , now : Date.Date
    , error : Maybe String
    , currentIssues : Maybe (List Issue)
    , iceboxIssues : Maybe (List Issue)
    , closedIssues : Maybe (List Issue)
    , milestones : Dict.Dict (String, String) (Dict.Dict String ExpandedMilestone)
    , pickMilestoneForIssue : Maybe Issue
    , lockedIssueNumber : String
    , highlightStory : String
    , newMilestoneTitle : String
    , newIssueTitle : String
    , needFocus : Bool
    , addIssueToColumn : Column
    , addIssueToMilestone : String
    , filter : Filter
    , filterStoriesBy : String
    , etags : Dict.Dict String (String, String)
    , searchTerms : String
    , searchResults : RemoteData (List Issue)
    , didSwitch : Bool
    , persistentData : PersistentData
    , repos : Pages.Repos.Model
    }


type RemoteData a
    = NotRequested
    | Loading
    | Loaded a


type CachedData
    = CachedData String String String
    | NotCached String


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
