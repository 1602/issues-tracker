module Route exposing (Route, Route(..), parseHash, href)

-- import Base exposing (..)
import UrlParser exposing (Parser, (</>), s, int, string, map, oneOf)
import Navigation exposing (Location)
import Html.Attributes as Attr
import Html exposing (Attribute)


type Route
    = Stories String String
    | Story String String String
    | Milestones String String
    | Settings String String
    | Repos


route : Parser (Route -> a) a
route =
    let
        repo =
            string </> string
    in
        oneOf
            [ map Stories <| repo </> s "stories"
            , map Milestones <| repo </> s  "milestones"
            , map Story <| repo </> s "stories" </> string
            , map Settings <| repo </> s  "settings"
            , map Repos <| s ""
            ]

parseHash : Location -> Maybe Route
parseHash loc =
    UrlParser.parseHash route loc


routeToString : Route -> String
routeToString page =
    case page of
        Repos -> 
            "#/"

        Milestones user repo ->
            "#/" ++ user ++ "/" ++ repo ++ "/milestones"

        Settings user repo ->
            "#/" ++ user ++ "/" ++ repo ++ "/settings"

        Stories user repo ->
            "#/" ++ user ++ "/" ++ repo ++ "/stories"

        Story user repo id ->
            "#/" ++ user ++ "/" ++ repo ++ "/stories/" ++ id


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)

