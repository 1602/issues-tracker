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


replaceHashesWithLinks : String -> String -> String
replaceHashesWithLinks repo =
    let
        expr =
            regex "\\s#(\\d+)"

        replaceHashWithLink match =
            case match.submatches of
                (Just issue) :: [] ->
                    " [#" ++ issue ++ "](#/" ++ repo ++ "/stories/" ++ issue ++ ")"

                _ ->
                    match.match
    in
        replace
            All
            expr
            replaceHashWithLink


replaceLinksWithIssues : String -> String
replaceLinksWithIssues =
    let
        replaceLinkWithIssue match =
            case match.submatches of
                (Just org) :: (Just repo) :: (Just issue) :: [] ->
                    " [" ++ org ++ "/" ++ repo ++ "#" ++ issue ++ "](#/" ++ org ++ "/" ++ repo ++ "/stories/" ++ issue ++ ")"

                _ ->
                    match.match

        expr =
            regex "\\shttps:\\/\\/github.com\\/(.+?)\\/(.+?)\\/issues\\/(\\d+)"
    in
        replace
            All
            expr
            replaceLinkWithIssue


replaceNamedLinksWithIssues : String -> String
replaceNamedLinksWithIssues =
    let
        replaceNamedLinkWithIssue match =
            case match.submatches of
                (Just org) :: (Just repo) :: (Just issue) :: [] ->
                    "(#/" ++ org ++ "/" ++ repo ++ "/stories/" ++ issue ++ ")"

                _ ->
                    match.match

        expr =
            regex "\\(\\s*https:\\/\\/github.com\\/(.+?)\\/(.+?)\\/issues\\/(\\d+)\\s*\\)"
    in
        replace
            All
            expr
            replaceNamedLinkWithIssue
