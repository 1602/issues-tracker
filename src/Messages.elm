module Messages exposing (..)

import Models exposing (..)
-- import Base exposing (..)
import Http exposing (Error, Response)
import Date
import Time
import Navigation exposing (Location)

type Msg
    = NoOp
    | LoadMilestones (Result Error (List (List Issue)))
    | CurrentDate Date.Date
    | CurrentTime Time.Time
    | UrlChange Location
    | IssuesLoaded Column (Result Error (List Issue))
    | CopyText String

