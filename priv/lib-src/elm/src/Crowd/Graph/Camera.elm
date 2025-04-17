module Crowd.Graph.Camera exposing
    ( Camera
    , custom
    , fixedArea
    , fixedHeight
    , fixedWidth
    , getViewSize
    , view
    , viewportToGameCoordinates
    , viewportToGameCoordinatesFloat
    )

import Math.Matrix4 as Mat4 exposing (Mat4)


type Size
    = Width Float
    | Height Float
    | Area Float
    | Custom (( Float, Float ) -> ( Float, Float ))


type alias Camera =
    { size : Size
    , position : ( Float, Float )
    }


fixedWidth : Float -> ( Float, Float ) -> Camera
fixedWidth w pos =
    { size = Width w, position = pos }


fixedHeight : Float -> ( Float, Float ) -> Camera
fixedHeight h pos =
    { size = Height h, position = pos }


fixedArea : Float -> ( Float, Float ) -> Camera
fixedArea a pos =
    { size = Area a, position = pos }


custom : (( Float, Float ) -> ( Float, Float )) -> ( Float, Float ) -> Camera
custom fn pos =
    { size = Custom fn, position = pos }


view : Camera -> ( Float, Float ) -> Mat4
view ({ position } as camera) size =
    let
        ( x, y ) =
            position

        ( w_, h_ ) =
            getViewSize size camera

        ( w, h ) =
            ( w_ / 2, h_ / 2 )

        ( ( l, r ), ( d, u ) ) =
            ( ( x - w, x + w ), ( y - h, y + h ) )
    in
    Mat4.makeOrtho2D l r d u


getViewSize : ( Float, Float ) -> Camera -> ( Float, Float )
getViewSize ( w, h ) { size } =
    case size of
        Width x ->
            ( x, x * h / w )

        Height y ->
            ( y * w / h, y )

        Area a ->
            ( sqrt (a * w / h), sqrt (a * h / w) )

        Custom fn ->
            fn ( w, h )


viewportToGameCoordinates : Camera -> ( Int, Int ) -> ( Int, Int ) -> ( Float, Float )
viewportToGameCoordinates camera ( width, height ) ( x, y ) =
    let
        ( ( screenLeft, screenRight ), ( screenTop, screenBottom ) ) =
            ( ( 0, toFloat width ), ( 0, toFloat height ) )

        ( gameWidth, gameHeight ) =
            getViewSize ( toFloat width, toFloat height ) camera

        ( cameraXOffset, cameraYOffset ) =
            camera.position

        ( ( viewLeft, viewRight ), ( viewTop, viewBottom ) ) =
            ( ( -(gameWidth / 2) + cameraXOffset
              , (gameWidth / 2) + cameraXOffset
              )
            , ( (gameHeight / 2) + cameraYOffset
              , -(gameHeight / 2) + cameraYOffset
              )
            )
    in
    ( viewLeft + ((toFloat x - screenLeft) / (screenRight - screenLeft) * (viewRight - viewLeft))
    , viewTop + ((toFloat y - screenTop) / (screenBottom - screenTop) * (viewBottom - viewTop))
    )


viewportToGameCoordinatesFloat : Camera -> ( Int, Int ) -> ( Float, Float ) -> ( Float, Float )
viewportToGameCoordinatesFloat camera ( width, height ) ( x, y ) =
    let
        ( ( screenLeft, screenRight ), ( screenTop, screenBottom ) ) =
            ( ( 0, toFloat width ), ( 0, toFloat height ) )

        ( gameWidth, gameHeight ) =
            getViewSize ( toFloat width, toFloat height ) camera

        ( cameraXOffset, cameraYOffset ) =
            camera.position

        ( ( viewLeft, viewRight ), ( viewTop, viewBottom ) ) =
            ( ( -(gameWidth / 2) + cameraXOffset
              , (gameWidth / 2) + cameraXOffset
              )
            , ( (gameHeight / 2) + cameraYOffset
              , -(gameHeight / 2) + cameraYOffset
              )
            )
    in
    ( viewLeft + ((x - screenLeft) / (screenRight - screenLeft) * (viewRight - viewLeft))
    , viewTop + ((y - screenTop) / (screenBottom - screenTop) * (viewBottom - viewTop))
    )
