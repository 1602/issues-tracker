module Route exposing (Route, Route(..), parseHash)

-- import Base exposing (..)
import UrlParser exposing (Parser, (</>), s, int, string, map, oneOf)
import Navigation exposing (Location)

type Route
    = IssuesIndex String String
    | Story String String String
    | MilestonesIndex String String


route : Parser (Route -> a) a
route =
    let
        repo =
            1--(string </> string </> identity)
    in
        oneOf
            [ map IssuesIndex (string </> string </> s "stories")
            , map MilestonesIndex (string </> string </> s  "milestones")
            , map Story (string </> string </> s "stories" </> string)
            ]

parseHash : Location -> Maybe Route
parseHash loc =
    UrlParser.parseHash route loc
