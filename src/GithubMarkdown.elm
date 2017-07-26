module GithubMarkdown exposing (ghMd)

import Html
import Markdown exposing (toHtml)
import Regex exposing (Regex, regex, replace, HowMany(All))


ghMd : ( String, String ) -> String -> Html.Html msg
ghMd ( u, r ) str =
    str
        |> replaceLinksWithIssues
        |> replaceNamedLinksWithIssues
        |> replaceHashesWithLinks (u ++ "/" ++ r)
        |> toHtml []


hashWithNumber : Regex
hashWithNumber =
    regex "\\s#(\\d+)"


replaceHashesWithLinks : String -> String -> String
replaceHashesWithLinks repo =
    let
        replaceHashWithLink match =
            case match.submatches of
                (Just issue) :: [] ->
                    " [#" ++ issue ++ "](#/" ++ repo ++ "/stories/" ++ issue ++ ")"

                _ ->
                    match.match
    in
        replace
            All
            hashWithNumber
            replaceHashWithLink


linkToIssue : Regex
linkToIssue =
    regex "\\shttps:\\/\\/github.com\\/(.+?)\\/(.+?)\\/issues\\/(\\d+)"


replaceLinksWithIssues : String -> String
replaceLinksWithIssues =
    let
        replaceLinkWithIssue match =
            case match.submatches of
                (Just org) :: (Just repo) :: (Just issue) :: [] ->
                    " [" ++ org ++ "/" ++ repo ++ "#" ++ issue ++ "](#/" ++ org ++ "/" ++ repo ++ "/stories/" ++ issue ++ ")"

                _ ->
                    match.match
    in
        replace
            All
            linkToIssue
            replaceLinkWithIssue


namedLinkToIssue : Regex
namedLinkToIssue =
    regex "\\(\\s*https:\\/\\/github.com\\/(.+?)\\/(.+?)\\/issues\\/(\\d+)\\s*\\)"


replaceNamedLinksWithIssues : String -> String
replaceNamedLinksWithIssues =
    let
        replaceNamedLinkWithIssue match =
            case match.submatches of
                (Just org) :: (Just repo) :: (Just issue) :: [] ->
                    "(#/" ++ org ++ "/" ++ repo ++ "/stories/" ++ issue ++ ")"

                _ ->
                    match.match
    in
        replace
            All
            namedLinkToIssue
            replaceNamedLinkWithIssue
