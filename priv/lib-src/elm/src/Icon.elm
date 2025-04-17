module Icon exposing (Icon(..), view)

import Html exposing (..)
import Html.Attributes exposing (..)


type Icon
    = Like
    | Flag
    | Remarks
    | Pen
    | Close


view : Icon -> Html msg
view icon =
    let
        wrap className =
            i [ class className ] []
    in
    wrap <|
        case icon of
            Like ->
                "icon--like"

            Close ->
                "icon--close"

            Flag ->
                "icon--flag"

            Remarks ->
                "icon--remarks"

            Pen ->
                "icon--pen"
