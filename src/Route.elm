module Route exposing (Route, Route(..), parseHash)

-- import Base exposing (..)
import UrlParser exposing (Parser, (</>), s, int, string, map, oneOf)
import Navigation exposing (Location)

type Route
    = IssuesIndex String String
    | Story String String String
    | MilestonesIndex String String
    | Settings String String


route : Parser (Route -> a) a
route =
    let
        repo =
            string </> string
    in
        oneOf
            [ map IssuesIndex <| repo </> s "stories"
            , map MilestonesIndex <| repo </> s  "milestones"
            , map Story <| repo </> s "stories" </> string
            , map Settings <| repo </> s  "settings"
            ]

parseHash : Location -> Maybe Route
parseHash loc =
    UrlParser.parseHash route loc
