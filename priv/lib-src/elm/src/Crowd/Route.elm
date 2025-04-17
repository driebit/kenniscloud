module Crowd.Route exposing
    ( CrowdLink
    , Mode(..)
    , Route(..)
    , fromUrl
    , link
    , toEventUrl
    , toUrl
    , toUrlWithHost
    , unCrowdLink
    )

import Ginger.Id as Id
import Html exposing (..)
import Html.Attributes exposing (..)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query


type CrowdLink
    = CrowdLink String


unCrowdLink : CrowdLink -> String
unCrowdLink (CrowdLink crowdLink) =
    crowdLink


crowdLinkToQuery : CrowdLink -> Builder.QueryParameter
crowdLinkToQuery =
    Builder.string "crowdlink" << unCrowdLink


type Route
    = Crowd Id.ResourceId Mode (Maybe CrowdLink)


type Mode
    = Present
    | Normal


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse parseUrl


parseUrl : Parser (Route -> a) a
parseUrl =
    Parser.oneOf
        [ Parser.map (\id crowdLink -> Crowd id Normal crowdLink) <|
            Parser.s "crowd"
                </> Id.fromUrl
                <?> parseCrowdLink
        , Parser.map Crowd <|
            Parser.s
                "crowd"
                </> Id.fromUrl
                </> parseMode
                <?> parseCrowdLink
        ]


parseMode : Parser (Mode -> a) a
parseMode =
    Parser.oneOf
        [ Parser.map Present (Parser.s "present")
        , Parser.map Normal (Parser.s "normal")
        ]


parseCrowdLink : Query.Parser (Maybe CrowdLink)
parseCrowdLink =
    Query.map (Maybe.map CrowdLink) <| Query.string "crowdlink"


toUrlWithHost : String -> Route -> String
toUrlWithHost host route =
    case route of
        Crowd id mode crowdLink ->
            Builder.crossOrigin
                host
                [ "crowd", Id.toString id, modeToString mode ]
                (List.filterMap identity
                    [ Maybe.map crowdLinkToQuery crowdLink ]
                )


toUrl : Route -> String
toUrl route =
    case route of
        Crowd id mode crowdLink ->
            Builder.absolute
                [ "crowd", Id.toString id, modeToString mode ]
                (List.filterMap identity
                    [ Maybe.map crowdLinkToQuery crowdLink ]
                )


toEventUrl : Maybe Route -> String
toEventUrl route =
    case route of
        Just (Crowd id _ _) ->
            Builder.absolute
                [ "page", Id.toString id ]
                []

        Nothing ->
            Builder.absolute [] []


modeToString : Mode -> String
modeToString mode =
    case mode of
        Present ->
            "present"

        Normal ->
            "normal"


link : Route -> List (Attribute msg) -> List (Html msg) -> Html msg
link route attrs children =
    a (href (toUrl route) :: attrs) children
