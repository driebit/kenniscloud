module Crowd.Graph.Simulation exposing (EmptyEdge, Model, attract, centerOfMass, init, span, tick, withGraph)

import Dict exposing (Dict)
import Force
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import List.Extra as List
import Time exposing (Posix)


type alias Model a =
    { graph : Graph (Entity a) ()
    , simulation : Force.State NodeId
    , connections : List EmptyEdge
    }


type alias Entity a =
    Force.Entity NodeId { value : a }


type alias EmptyEdge =
    { source : Int
    , target : Int
    , targetDistance : Int
    , render : Bool
    }


init : Model a
init =
    { graph = Graph.empty
    , simulation = Force.simulation []
    , connections = []
    }


modifyEntity : NodeId -> (Entity a -> Entity a) -> Model a -> Model a
modifyEntity nodeId modify model =
    { model
        | graph =
            Graph.update
                nodeId
                (Maybe.map
                    (\({ node } as nodeCtx) ->
                        { nodeCtx
                            | node = { node | label = modify node.label }
                        }
                    )
                )
                model.graph
    }


attract : NodeId -> Float -> Model a -> Model a
attract nodeId strength model =
    case Graph.get nodeId model.graph of
        Just attractor ->
            let
                changedModel =
                    List.foldr
                        (\neighbourId ->
                            modifyEntity neighbourId
                                (\entity ->
                                    { entity
                                        | vx = entity.vx + (attractor.node.label.x - entity.x) * strength
                                        , vy = entity.vy + (attractor.node.label.y - entity.y) * strength
                                    }
                                )
                        )
                        model
                    <|
                        Graph.alongIncomingEdges attractor
            in
            { changedModel | simulation = Force.reheat changedModel.simulation }

        Nothing ->
            model


tick : Posix -> Model a -> Model a
tick t model =
    let
        ( newState, list ) =
            Force.tick model.simulation (List.map .label (Graph.nodes model.graph))
    in
    { model
        | graph = updateGraphWithList model.graph list
        , simulation = newState
    }


updateGraphWithList : Graph (Entity a) () -> List (Entity a) -> Graph (Entity a) ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


updateContextWithValue : NodeContext (Entity a) () -> Entity a -> NodeContext (Entity a) ()
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


countEdges : List EmptyEdge -> Dict Int Int
countEdges edges =
    let
        updateCount { source } acc =
            if Dict.member source acc then
                Dict.update source (Maybe.map ((+) 1)) acc

            else
                Dict.insert source 1 acc
    in
    List.foldl updateCount Dict.empty edges


withGraph : List (Node (Entity a)) -> List EmptyEdge -> Model a -> Model a
withGraph nodes edges model =
    let
        outgoingEdgeCount =
            countEdges edges

        -- nodesWithEdgeCount =
        --     List.map (\node -> { node | edgeCount = Dict.get node.id outgoingEdgeCount }) nodes
        graph =
            Graph.fromNodesAndEdges
                (List.map (\node -> Node node.id (Force.entity node.id node.label.value)) nodes)
                (List.map (\{ source, target } -> Edge source target ()) edges)

        graphSize =
            Graph.size graph

        simulation =
            Force.simulation
                [ Force.customLinks 1 (List.map (\{ source, target, targetDistance } -> { source = source, target = target, distance = toFloat targetDistance, strength = Just 0.3 }) edges)
                , Force.manyBodyStrength simulationStrength (List.map .id (Graph.nodes graph))
                , Force.center 0 0
                ]
    in
    { model
        | graph = graph
        , connections = edges
        , simulation = simulation
    }


simulationStrength : Float
simulationStrength =
    -5000


span : Graph (Entity a) () -> Float
span graph =
    let
        ( cx, cy ) =
            centerOfMass graph

        nodes =
            Graph.nodes graph

        dist =
            List.map
                (\node -> ( sqrt <| (node.label.x - cx) ^ 2 + (node.label.y - cy) ^ 2, node ))
                nodes
                |> List.sortBy Tuple.first
                |> List.reverse
                |> List.take 3
                |> List.map Tuple.first
                |> List.foldr (+) 0
                |> (\x -> x / 3)
    in
    dist * 2


centerOfMass : Graph (Entity a) () -> ( Float, Float )
centerOfMass graph =
    let
        nodes =
            Graph.nodes graph
    in
    if List.length nodes > 0 then
        List.map (\node -> ( node.label.x, node.label.y )) nodes
            |> List.foldr (\( ax, ay ) ( bx, by ) -> ( ax + bx, ay + by )) ( 0, 0 )
            |> (\( x, y ) -> ( x / toFloat (List.length nodes), y / toFloat (List.length nodes) ))

    else
        ( 0, 0 )
