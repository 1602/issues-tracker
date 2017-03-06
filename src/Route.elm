module Route exposing (Route, Route(..), parseHash)

import Base exposing (..)
import UrlParser exposing (Parser, (</>), s, int, string, map, oneOf)
import Navigation exposing (Location)

type Route
    = IssuesIndex


route : Parser (Route -> a) a
route =
    oneOf
        [ map IssuesIndex (s "issues")
        ]

parseHash : Location -> Maybe Route
parseHash loc =
    UrlParser.parseHash route loc
