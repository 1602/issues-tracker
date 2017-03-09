module Route exposing (Route, Route(..), parseHash)

import Base exposing (..)
import UrlParser exposing (Parser, (</>), s, int, string, map, oneOf)
import Navigation exposing (Location)

type Route
    = IssuesIndex
    | MilestonesIndex


route : Parser (Route -> a) a
route =
    oneOf
        [ map IssuesIndex (s "issues")
        , map MilestonesIndex (s "milestones")
        ]

parseHash : Location -> Maybe Route
parseHash loc =
    UrlParser.parseHash route loc
