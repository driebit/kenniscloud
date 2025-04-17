module Crowd.Graph.Render exposing
    ( CanvasDimensions
    , RenderableGraph
    , view
    )

import Arc2d
import Array
import Circle2d
import Crowd.Graph.Camera as Camera exposing (Camera)
import Crowd.Graph.TextureStore as TextureStore exposing (TextureStore, TextureUrl)
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
import Html exposing (..)
import Html.Attributes exposing (..)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Point2d exposing (Point2d)
import Point3d
import Polygon2d
import Polyline2d
import SketchPlane3d
import TriangularMesh
import Units.Pixels exposing (Pixels)
import Units.Quantity as Quantity exposing (Quantity)
import WebGL exposing (Mesh, Shader)
import WebGL.Settings as WebGL
import WebGL.Texture exposing (Texture)


type UserCoords
    = UserCoords


{-| Represents a bubble in the graph
-}
type alias Bubble =
    { label : Maybe String
    , image : TextureUrl
    , position : Point2d Pixels UserCoords
    , radius : Quantity Float UserCoords
    , highlight : Bool
    }


type alias CanvasDimensions =
    { width : Int
    , height : Int
    }


type alias Connection =
    { a : Point2d Pixels UserCoords
    , b : Point2d Pixels UserCoords
    , highlight : Bool
    }


type alias RenderableGraph =
    { nodes : List Bubble
    , connections : List Connection
    , camera : Camera
    }


view : List (Attribute msg) -> CanvasDimensions -> TextureStore -> RenderableGraph -> Html msg
view attrs dimensions store graph =
    let
        bubbles =
            List.map (bubbleEntity dimensions graph.camera) <|
                List.filterMap
                    (\bubble ->
                        Maybe.map (Tuple.pair bubble)
                            (TextureStore.get bubble.image store)
                    )
                    graph.nodes

        connections =
            List.map (lineEntity dimensions graph.camera) graph.connections

        bubbleLabel bubble =
            if bubble.highlight then
                Maybe.map
                    (\label ->
                        textBox label
                            (List.foldr Mat4.mul
                                Mat4.identity
                                [ Mat4.makeTranslate3 (toFloat dimensions.width / 2 - 250) (-(toFloat dimensions.height) / 2 + 20) 0
                                , Mat4.makeScale3 (toFloat dimensions.width / 2) (toFloat dimensions.height / 2) 1
                                , Camera.view graph.camera ( toFloat dimensions.width, toFloat dimensions.height )
                                , Mat4.makeTranslate3 (Point2d.xCoordinate bubble.position |> Quantity.unwrap) (Quantity.unwrap (Point2d.yCoordinate bubble.position) - Quantity.unwrap bubble.radius) 0
                                , Mat4.makeScale3 0.8 0.8 1
                                ]
                            )
                    )
                    bubble.label

            else
                Nothing
    in
    div []
        [ div
            [ class "crowd-text-box-container"
            , style "width" (String.fromInt dimensions.width ++ "px")
            , style "height" (String.fromInt dimensions.height ++ "px")
            ]
            (List.filterMap bubbleLabel graph.nodes)
        , WebGL.toHtmlWith
            [ WebGL.depth 1, WebGL.alpha True ]
            ([ width (dimensions.width * 2)
             , height (dimensions.height * 2)
             , style "width" (String.fromInt dimensions.width ++ "px")
             , style "height" (String.fromInt dimensions.height ++ "px")
             , class "crowd-canvas"
             ]
                ++ attrs
            )
            (bubbles ++ connections)
        ]



-- RENDERING


bubbleEntity : CanvasDimensions -> Camera -> ( Bubble, Texture ) -> WebGL.Entity
bubbleEntity dimensions camera ( { position, radius, highlight }, tex ) =
    let
        uniforms =
            { camera = Camera.view camera ( toFloat dimensions.width, toFloat dimensions.height )
            , texture = tex
            , radius = Quantity.unwrap radius
            , size = Vec2.vec2 (toFloat dimensions.width) (toFloat dimensions.height)
            , highlight =
                -- Elm doesn't support Bool types for shaders yet, so this is a workaround
                -- See https://github.com/elm/compiler/issues/1970
                if highlight then
                    1.0

                else
                    0.0
            , transform =
                Mat4.identity
                    |> Mat4.translate3
                        (Point2d.xCoordinate position |> Quantity.unwrap)
                        (Point2d.yCoordinate position |> Quantity.unwrap)
                        0
            }
    in
    WebGL.entity bubbleVertexShader bubbleFragmentShader circle100 uniforms


lineEntity : CanvasDimensions -> Camera -> Connection -> WebGL.Entity
lineEntity dimensions camera { a, b, highlight } =
    let
        toVec point =
            Vec2.vec2 (Point2d.xCoordinate point |> Quantity.unwrap) (Point2d.yCoordinate point |> Quantity.unwrap)

        uniforms =
            { camera = Camera.view camera ( toFloat dimensions.width, toFloat dimensions.height )
            , a = toVec a
            , b = toVec b
            , highlight =
                -- Elm doesn't support Bool types for shaders yet, so this is a workaround
                -- See https://github.com/elm/compiler/issues/1970
                if highlight then
                    1.0

                else
                    0.0
            }
    in
    WebGL.entity lineVertexShader lineFragmentShader box1 uniforms



-- TEXT RENDERING


objectMatrix3d : { m11 : Float, m21 : Float, m31 : Float, m41 : Float, m12 : Float, m22 : Float, m32 : Float, m42 : Float, m13 : Float, m23 : Float, m33 : Float, m43 : Float, m14 : Float, m24 : Float, m34 : Float, m44 : Float } -> String
objectMatrix3d { m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44 } =
    [ m11, m21, m31, m41, -m12, -m22, -m32, -m42, m13, m23, m33, m43, m14, m24, m34, m44 ]
        |> List.map String.fromFloat
        |> List.intersperse ","
        |> List.foldr (++) ""
        |> (\s -> "matrix3d(" ++ s ++ ")")


textBox : String -> Mat4 -> Html msg
textBox label matrix =
    div
        [ class "crowd-text-box-wrapper"
        , style "transform" ("scale3d(1, -1, 1) " ++ objectMatrix3d (Mat4.toRecord matrix))
        ]
        [ span
            [ class "crowd-text-box"
            ]
            [ text label ]
        ]



-- MESHES


circle100 : Mesh Vertex
circle100 =
    circleMesh 100


box1 : Mesh Vertex
box1 =
    let
        a =
            Vertex (Vec3.vec3 -1 -1 0)

        b =
            Vertex (Vec3.vec3 -1 1 0)

        c =
            Vertex (Vec3.vec3 1 -1 0)

        d =
            Vertex (Vec3.vec3 1 1 0)
    in
    WebGL.triangles
        [ ( a, b, c )
        , ( b, c, d )
        ]


toVec3 : Point2d Pixels UserCoords -> Vec3
toVec3 =
    Point3d.toVec3 << Point3d.on SketchPlane3d.xy


{-| Creates a mesh of a circle using polygons given a radius
-}
circleMesh : Float -> Mesh Vertex
circleMesh radius =
    let
        circle =
            Circle2d.withRadius (Quantity.float radius) Point2d.origin
                |> Circle2d.toArc
                |> Arc2d.toPolyline { maxError = Quantity.float 0.1 }
                |> Polyline2d.vertices
                |> Polygon2d.singleLoop
                |> Polygon2d.triangulate
    in
    WebGL.indexedTriangles
        (Array.toList << Array.map (\p -> { position = Point3d.toVec3 <| Point3d.on SketchPlane3d.xy p }) <| TriangularMesh.vertices circle)
        (TriangularMesh.faceIndices circle)


type alias Vertex =
    { position : Vec3
    }


type alias BubbleUniforms a =
    { a
        | camera : Mat4
        , size : Vec2
        , highlight : Float
    }



-- SHADERS


bubbleVertexShader : Shader Vertex (BubbleUniforms { a | radius : Float, transform : Mat4 }) { vcoord : Vec2 }
bubbleVertexShader =
    [glsl|
        attribute vec3 position;
        uniform mat4 camera;
        uniform mat4 transform;
        uniform vec2 size;
        uniform float radius;
        varying vec2 vcoord;

        void main () {
            const float source_poly_radius = 100.0;

            gl_Position = camera * transform * vec4(position.x * radius / source_poly_radius, position.y * radius / source_poly_radius, position.z * radius / source_poly_radius, 1.0);
            vcoord = vec2(position.x / source_poly_radius / 2.0 + 0.5, position.y / source_poly_radius / 2.0 + 0.5);
        }
    |]


bubbleFragmentShader : Shader {} (BubbleUniforms { a | texture : Texture }) { vcoord : Vec2 }
bubbleFragmentShader =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vcoord;
        uniform float highlight;

        void main () {
            const float hidden_alpha = 0.25;
            const float shown_alpha = 1.0;


            float alpha = highlight == 1.0 ? shown_alpha : hidden_alpha;
            gl_FragColor = vec4(texture2D(texture, vcoord).xyz * alpha, alpha);
        }
    |]


type alias LineUniforms a =
    { a
        | camera : Mat4
        , a : Vec2
        , b : Vec2
        , highlight : Float
    }


lineFragmentShader : Shader {} (LineUniforms a) {}
lineFragmentShader =
    [glsl|
        precision mediump float;

        void main () {
            gl_FragColor = vec4(0.5, 0.5, 0.5, 1.0);
        }
    |]


lineVertexShader : Shader Vertex (LineUniforms a) {}
lineVertexShader =
    [glsl|
        attribute vec3 position;
        uniform mat4 camera;
        uniform vec2 a;
        uniform vec2 b;
        uniform float highlight;

        mat4 rotateZ(float psi){
            return mat4(
                vec4(cos(psi), -sin(psi), 0., 0.),
                vec4(sin(psi), cos(psi),  0., 0.),
                vec4(0.,       0.,        1., 0.),
                vec4(0.,       0.,        0., 1.)
            );
        }

        mat4 scale(float x, float y, float z){
            return mat4(
                vec4(x,   0.0, 0.0, 0.0),
                vec4(0.0, y,   0.0, 0.0),
                vec4(0.0, 0.0, z,   0.0),
                vec4(0.0, 0.0, 0.0, 1.0)
            );
        }

        mat4 translate(float x, float y, float z){
            return mat4(
                vec4(1.0, 0.0, 0.0, 0.0),
                vec4(0.0, 1.0, 0.0, 0.0),
                vec4(0.0, 0.0, 1.0, 0.0),
                vec4(x,   y,   z,   1.0)
            );
        }

        void main () {
            vec3 tA = vec3(a, 0.0);
            vec3 tB = vec3(b, 0.0);
            float delta = length(tB - tA);
            vec3 o = (tA + tB) * 0.5;
            float theta = atan(tA.y - o.y, tB.x - o.x);
            float thickness = highlight == 1.0 ? 2.0 : 0.5;
            mat4 transforms = camera * translate(o.x, o.y, 0.0) * rotateZ(theta) * scale(delta / 2.0, thickness, 1.0);
            gl_Position = transforms * vec4(position, 1.0);
        }
    |]
