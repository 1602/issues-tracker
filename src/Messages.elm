module Messages exposing (..)

import Models exposing (..)
-- import Base exposing (..)
import Http exposing (Error, Response)
import Date
import Time
import Navigation exposing (Location)

type Msg
    = NoOp
    | LoadMilestones (Result Error (List Milestone))
    | MilestoneIssuesLoaded String IssueState (Result Error (List Issue))
    | CurrentDate Date.Date
    | CurrentTime Time.Time
    | UrlChange Location
    | IssuesLoaded Column (Result Error (List Issue))
    | UnsetMilestone Milestone (Result Error Issue)
    | SetMilestone Issue Milestone
    | MilestoneSet Milestone (Result Error Issue)
    | IssueStarted Milestone (Result Error Issue)
    | IssueRestarted Milestone (Result Error Issue)
    | IssueFinished Milestone (Result Error Issue)
    | LoadUser (Result Error User)
    | IssueAction Issue String
    | CopyText String
    | EditAccessToken String
    | SaveAccessToken
    | DismissPlanningIssue
    | SelectStory Issue

