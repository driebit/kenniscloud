module Crowd.Graph.View.PersonCard exposing (dayCrowdView, view)

import Crowd.Assets as Assets
import Crowd.Request as Request
import Ginger.Id as Id
import Ginger.Translation as Translation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icon


linkToUser : Request.User -> Html msg -> Html msg
linkToUser user child =
    if user.isAnonymous then
        child

    else
        a [ href ("/page/" ++ Id.toString user.id) ]
            [ child ]


view : msg -> Request.User -> Html msg
view unselect user =
    let
        categoryName =
            if user.isAnonymous then
                "Tijdelijke deelnemer"

            else
                "Deelnemer"
    in
    div [ class "list__item person-card" ]
        [ button [ class "person-card-close", onClick unselect ] [ Icon.view Icon.Close ]
        , linkToUser user <|
            article []
                [ div [ class "list__item__image" ]
                    [ case Request.getAvatar user of
                        Just avatar ->
                            img [ src avatar ] []

                        Nothing ->
                            img [ src Assets.person ] []
                    ]
                , div [ class "list__item__content" ]
                    [ div [ class "list__item__title" ]
                        [ div [ class "category-of" ]
                            [ div [ class "category-of__cat" ]
                                [ text categoryName ]
                            ]
                        , h3 [] [ Translation.text Translation.NL user.title ]
                        ]
                    , p [] [ Translation.text Translation.NL user.summary ]
                    ]
                ]
        ]


dayCrowdView : msg -> Request.User -> Html msg
dayCrowdView unselect user =
    div [ class "list__item person-card" ]
        [ button [ class "person-card-close", onClick unselect ] [ Icon.view Icon.Close ]
        , article []
            [ div [ class "list__item__image" ]
                [ case Request.getAvatar user of
                    Just avatar ->
                        img [ src avatar ] []

                    Nothing ->
                        img [ src Assets.person ] []
                ]
            , div [ class "list__item__content" ]
                [ div [ class "list__item__title" ]
                    [ div [ class "category-of" ]
                        [ div [ class "category-of__cat" ]
                            [ text "Deelnemer" ]
                        ]
                    , h3 [] [ Translation.text Translation.NL user.title ]
                    ]
                , p []
                    [ table []
                        [ tr []
                            [ td [] []
                            , td []
                                (user.tags
                                    |> List.map (\tag -> Translation.text Translation.NL tag.title)
                                    |> List.intersperse (text ", ")
                                )
                            ]
                        ]
                    ]
                , case user.email of
                    Nothing ->
                        text ""

                    Just email ->
                        p [] [ linkEmail email [] [ text email ] ]
                ]
            ]
        ]


linkEmail : String -> List (Attribute msg) -> List (Html msg) -> Html msg
linkEmail url attributes content =
    a (href ("mailto:" ++ url) :: attributes) content
