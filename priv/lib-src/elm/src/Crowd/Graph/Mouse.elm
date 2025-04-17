module Crowd.Graph.Mouse exposing
    ( MouseMoveData
    , State
    , TouchData
    , delta
    , down
    , init
    , move
    , onMouseMove
    , onTouchEnd
    , onTouchMove
    , onTouchStart
    , position
    , touchEnd
    , touchMove
    , touchStart
    , up
    )

import DOM
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)


type alias State =
    { current : Maybe ( Int, Int )
    , last : Maybe ( Int, Int )
    , hasDragged : Bool
    , isDown : Bool
    }


init : State
init =
    { current = Nothing
    , last = Nothing
    , hasDragged = False
    , isDown = False
    }


down : State -> State
down state =
    { state
        | isDown = True
        , hasDragged = False
    }


delta : State -> ( Int, Int )
delta state =
    Maybe.withDefault ( 0, 0 ) <|
        Maybe.map2
            (\( cx, cy ) ( lx, ly ) -> ( cx - lx, cy - ly ))
            state.current
            state.last


up : State -> State
up state =
    { state
        | isDown = False
    }


move : MouseMoveData -> State -> State
move moveData state =
    let
        ( dx, dy ) =
            delta state
    in
    { state
        | current = Just (moveCoordinates moveData)
        , last = state.current
        , hasDragged = state.isDown && (state.hasDragged || abs dx > 3 || abs dy > 3)
    }


position : State -> ( Int, Int )
position state =
    Maybe.withDefault ( 0, 0 ) state.current


touchStart : TouchData -> State -> State
touchStart event state =
    { state
        | isDown = True
        , hasDragged = False
        , current = Just (touchCoordinates event)
        , last = state.current
    }


touchMove : TouchData -> State -> State
touchMove event state =
    { state
        | isDown = True
        , hasDragged = True
        , current = Just (touchCoordinates event)
        , last = state.current
    }


touchEnd : TouchData -> State -> State
touchEnd event state =
    { state
        | isDown = False
        , hasDragged = False
        , current = Just (touchCoordinates event)
        , last = state.current
    }


type alias MouseMoveData =
    { offsetX : Int
    , offsetY : Int
    , offsetHeight : Float
    , offsetWidth : Float
    }


type alias TouchData =
    { offsetX : Int
    , offsetY : Int
    }


moveCoordinates : MouseMoveData -> ( Int, Int )
moveCoordinates data =
    ( data.offsetX, data.offsetY )


decoder : Decoder MouseMoveData
decoder =
    Decode.map4 MouseMoveData
        (Decode.at [ "offsetX" ] Decode.int)
        (Decode.at [ "offsetY" ] Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.float)
        (Decode.at [ "target", "offsetWidth" ] Decode.float)


onMouseMove : (MouseMoveData -> msg) -> Attribute msg
onMouseMove toMsg =
    on "mousemove" (Decode.map toMsg decoder)


touchDecoder : Decoder TouchData
touchDecoder =
    Decode.map2
        (\rect offsets ->
            let
                len =
                    List.length offsets

                ( ox, oy ) =
                    Maybe.withDefault ( 0, 0 ) <| List.head offsets
            in
            if len >= 2 then
                Decode.fail "multi touch"

            else
                Decode.succeed
                    { offsetX = round <| ox - rect.left
                    , offsetY = round <| oy - rect.top
                    }
        )
        (DOM.target DOM.boundingClientRect)
        (Decode.field "changedTouches" <|
            dynamicListOf <|
                Decode.map2 Tuple.pair
                    (Decode.field "pageX" Decode.float)
                    (Decode.field "pageY" Decode.float)
        )
        |> Decode.andThen identity


onTouchStart : (TouchData -> msg) -> Attribute msg
onTouchStart toMsg =
    preventDefaultOn "touchstart" (Decode.map (\e -> ( toMsg e, True )) touchDecoder)


onTouchMove : (TouchData -> msg) -> Attribute msg
onTouchMove toMsg =
    preventDefaultOn "touchmove" (Decode.map (\e -> ( toMsg e, True )) touchDecoder)


onTouchEnd : (TouchData -> msg) -> Attribute msg
onTouchEnd toMsg =
    preventDefaultOn "touchend" (Decode.map (\e -> ( toMsg e, True )) touchDecoder)


touchCoordinates : TouchData -> ( Int, Int )
touchCoordinates touchEvent =
    ( touchEvent.offsetX, touchEvent.offsetY )


all : List (Decoder a) -> Decoder (List a)
all =
    List.foldr (Decode.map2 (::)) (Decode.succeed [])


dynamicListOf : Decoder a -> Decoder (List a)
dynamicListOf itemDecoder =
    let
        decodeN n =
            List.range 0 (n - 1)
                |> List.map decodeOne
                |> all

        decodeOne n =
            Decode.field (String.fromInt n) itemDecoder
    in
    Decode.field "length" Decode.int
        |> Decode.andThen decodeN
