module Route exposing (Route, Route(..), parseHash)

-- import Base exposing (..)
import UrlParser exposing (Parser, (</>), s, int, string, map, oneOf)
import Navigation exposing (Location)

type Route
    = IssuesIndex
    | Story String
    | MilestonesIndex


route : Parser (Route -> a) a
route =
    oneOf
        [ map IssuesIndex (s "stories")
        , map MilestonesIndex (s "milestones")
        , map Story (s "stories" </> string)
        ]

parseHash : Location -> Maybe Route
parseHash loc =
    UrlParser.parseHash route loc
