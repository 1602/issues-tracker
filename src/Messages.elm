module Messages exposing (..)

import Models exposing (..)
-- import Base exposing (..)
import Http exposing (Error, Response)
import Date
import Time
import Navigation exposing (Location)

type Msg
    = NoOp
    | LoadMilestones String
    | MilestoneIssuesLoaded String IssueState String
    | CurrentDate Date.Date
    | CurrentTime Time.Time
    | UrlChange Location
    | IssuesLoaded Column String
    | UnsetMilestone Milestone (Result Error Issue)
    | SetMilestone Issue Milestone
    | MilestoneSet Milestone (Result Error Issue)
    | MilestoneCreated (Result Error Milestone)
    | IssueStarted (Maybe Milestone) (Result Error Issue)
    | IssueRestarted (Maybe Milestone) (Result Error Issue)
    | IssueFinished (Maybe Milestone) (Result Error Issue)
    | LoadUser (Result Error User)
    | IssueAction Issue String
    | CopyText String
    | EditAccessToken String
    | SaveAccessToken
    | DismissPlanningIssue
    | SelectStory Issue
    | EditNewMilestoneTitle String
    | CreateNewMilestone
    | StoryFocused
    | UrgentIssueAdded (Result Error Issue)
    | CreateStory Column
    | StoryCreated Column (Maybe Milestone) (Result Error Issue)
    | EditNewStoryTitle String
    | ShowIssueCreationForm Column String
    | ChangeFilter String
    | HideColumn Column
    | ReopenColumn Column
    | PinMilestone String
    | FilterStories String
    | FetchComplete (String -> Msg) (Result Error CachedData)
    | SettingsMsgProxy SettingsMsg
    | NavigateToIssue (String, String)
    | SearchIssues
    | ChangeSearchTerms String
    | IssuesSearchResults String
    | ToggleSaveSearch
    | SearchBy String
    | ClearSearch

type SettingsMsg
    = ChangeDefaultRepositoryType String
    | UpdateDefaultRepository String
    | ChangeDoneLimit String
    | IgnoreIdeas
