module GithubMarkdown exposing (ghMd)

import Html
import Markdown exposing (toHtml)
import Regex exposing (regex, replace, HowMany(..))

ghMd : String -> String -> Html.Html msg
ghMd repo str =
    str
        |> replaceLinksWithIssues
        |> replaceHashesWithLinks repo
        |> toHtml []

replaceHashesWithLinks : String -> String -> String
replaceHashesWithLinks repo =
    let
        expr =
            regex "\\s#(\\d+)"

        replaceHashWithLink match =
            case match.submatches of
                (Just issue) :: [] ->
                    " [#" ++ issue ++ "](https://github.com/" ++ repo ++ "/issues/" ++ issue ++ ")"
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
                    " [" ++ org ++ "/" ++ repo ++ "#" ++ issue ++ "](" ++ match.match ++ ")"
                _ ->
                    match.match

        expr =
            regex "\\shttps:\\/\\/github.com\\/(.+?)\\/(.+?)\\/issues\\/(\\d+)"
    in
        replace
            All
            expr
            replaceLinkWithIssue
