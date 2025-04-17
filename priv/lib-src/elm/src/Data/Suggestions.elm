module Data.Suggestions exposing
    ( Item
    , Suggestions(..)
    , focus
    , init
    , itemByIdFrom
    , itemFromId
    , itemFromJson
    , next
    , previous
    , toList
    )

import Ginger.Id exposing (ResourceId)
import Json.Decode as Decode exposing (Decoder)
import List.Zipper as Zipper exposing (Zipper)


{-| The search suggestions can have focus (be highlighted) or not.

If there are suggestions you're either focused or not. Transitions
between these states occur when you move up while being focused on
the first item or move down while being on the last.

    Empty -- no suggestions

    NoFocus String (List ( Id, String )) -- a non empty list

    HasFocus (Zipper String) -- a List where one elment has focus

-}
type Suggestions
    = NoFocus Item (List Item)
    | HasFocus (Zipper Item)
    | Empty


type alias Item =
    ( Maybe ResourceId, String )


itemFromJson : Decoder Item
itemFromJson =
    Decode.map2 Tuple.pair
        (Decode.field "id" (Decode.nullable Ginger.Id.fromJson))
        (Decode.field "title" Decode.string)


init : Item -> List Item -> Suggestions
init =
    NoFocus


focus : Item -> Suggestions -> Suggestions
focus val suggestions =
    case suggestions of
        Empty ->
            suggestions

        NoFocus x xs ->
            case Zipper.findFirst ((==) val) (Zipper.fromCons x xs) of
                Nothing ->
                    suggestions

                Just zipper_ ->
                    HasFocus zipper_

        HasFocus zipper ->
            case Zipper.findFirst ((==) val) zipper of
                Nothing ->
                    suggestions

                Just zipper_ ->
                    HasFocus zipper_


previous : Suggestions -> Suggestions
previous suggestions =
    case suggestions of
        Empty ->
            Empty

        NoFocus x xs ->
            HasFocus <|
                Zipper.last (Zipper.fromCons x xs)

        HasFocus zipper ->
            case Zipper.previous zipper of
                Nothing ->
                    HasFocus zipper

                Just zipper_ ->
                    HasFocus zipper_


next : Suggestions -> Suggestions
next suggestions =
    case suggestions of
        Empty ->
            Empty

        NoFocus x xs ->
            HasFocus <|
                Zipper.first (Zipper.fromCons x xs)

        HasFocus zipper ->
            case Zipper.next zipper of
                Nothing ->
                    HasFocus zipper

                Just zipper_ ->
                    HasFocus zipper_


toList : Suggestions -> List Item
toList suggestions =
    case suggestions of
        Empty ->
            []

        NoFocus x xs ->
            x :: xs

        HasFocus zipper ->
            Zipper.toList zipper


itemByIdFrom : Suggestions -> String -> Maybe Item
itemByIdFrom suggestions id =
    itemFromId id suggestions


itemFromId : String -> Suggestions -> Maybe Item
itemFromId id suggestions =
    suggestions
        |> toList
        |> List.filterMap
            (\( r, v ) ->
                case r of
                    Nothing ->
                        Nothing

                    Just ri ->
                        if Ginger.Id.toString ri == id then
                            Just ( r, v )

                        else
                            Nothing
            )
        |> List.head
