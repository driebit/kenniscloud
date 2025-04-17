module Remark.Viewer exposing (render)

import Html exposing (..)
import Markdown
import String.Interpolate exposing (interpolate)
import Types exposing (Mention)


render : String -> List Mention -> Html msg
render body mentions =
    let
        options =
            { githubFlavored = Just { tables = False, breaks = False }
            , defaultHighlighting = Nothing
            , sanitize = False
            , smartypants = False
            }
    in
    insertMentions body mentions
        |> convertLinks
        |> Markdown.toHtmlWith options []



-- CONVERT LINKS TO MARKDOWN LINKS


convertLinks : String -> String
convertLinks text =
    String.words text
        |> List.filter (startsWithOneOf [ "http://", "https://", "www." ])
        |> List.foldl convertLink text


convertLink : String -> String -> String
convertLink link text =
    let
        href =
            if not <| startsWithOneOf [ "http://", "https://" ] link then
                interpolate "http://{0}" [ link ]

            else
                link

        anchor =
            toMarkdownUrl link href
    in
    String.split link text
        |> String.join anchor



-- INSERT MENTION LINK(S) IN TEXT


insertMentions : String -> List Mention -> String
insertMentions text mentions =
    List.foldl insertMentionLink text mentions


insertMentionLink : Mention -> String -> String
insertMentionLink mention text =
    String.split ("@" ++ mention.userName) text
        |> String.join (toMarkdownUrl ("@" ++ mention.userName) mention.userUrl)



-- HELPERS


toMarkdownUrl : String -> String -> String
toMarkdownUrl text url =
    interpolate "[{0}]({1})" [ text, url ]


startsWithOneOf : List String -> String -> Bool
startsWithOneOf list text =
    List.any (\a -> String.startsWith a text) list
