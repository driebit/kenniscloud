module Crowd.Graph.View.IconButton exposing (Icon(..), view, viewLink, viewLinkNewTab)

import Crowd.Assets as Assets
import Crowd.Route as Route
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Icon
    = Plus
    | Minus
    | Fullscreen
    | PlusPresent
    | MinusPresent
    | FullscreenPresent


toUrl : Icon -> String
toUrl icon =
    case icon of
        Plus ->
            Assets.imageUrl "Icon_Plus.svg"

        Minus ->
            Assets.imageUrl "Icon_Minus.svg"

        Fullscreen ->
            Assets.imageUrl "Icon_Fullscreen.svg"

        PlusPresent ->
            Assets.imageUrl "Icon_Plus_Present.svg"

        MinusPresent ->
            Assets.imageUrl "Icon_Minus_Present.svg"

        FullscreenPresent ->
            Assets.imageUrl "Icon_Fullscreen_Present.svg"


view : Icon -> msg -> Html msg
view icon msg =
    button [ type_ "button", class "crowd-control", onClick msg ]
        [ span []
            [ img [ src (toUrl icon) ] []
            ]
        ]


viewLink : Icon -> Route.Route -> Html msg
viewLink icon route =
    a [ href (Route.toUrl route) ]
        [ button [ type_ "button", class "crowd-control" ]
            [ span []
                [ img [ src (toUrl icon) ] []
                ]
            ]
        ]


viewLinkNewTab : Icon -> Route.Route -> Html msg
viewLinkNewTab icon route =
    a [ href (Route.toUrl route), target "_blank" ]
        [ button [ type_ "button", class "crowd-control" ]
            [ span []
                [ img [ src (toUrl icon) ] []
                ]
            ]
        ]
