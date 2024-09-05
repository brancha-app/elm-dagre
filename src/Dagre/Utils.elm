module Dagre.Utils exposing (..)

import Graph as G
import IntDict exposing (IntDict)
import List.Extra as LE


intMin : Int
intMin =
    -2 ^ 31


intMax : Int
intMax =
    2 ^ 31 - 1


infinity : Float
infinity =
    2 ^ 31 - 1


type alias Coordinates =
    ( Float, Float )


type alias Edge =
    ( G.NodeId, G.NodeId )



{- This type represents a node with rank information -}


type alias RankedNode =
    { id : G.NodeId, rank : Int }



{- This type represents the other node of an edge
   This is used for generating initial orderings of nodes in each layer using BFS
-}


type alias OtherNodeOfEdge =
    { otherNode : G.NodeId
    , edge : { from : RankedNode, to : RankedNode } -- original from and to nodes of the edge
    }



{- This adjacency type is similar to Graph.Adjacency but it stores the current layers node as the key
   and the list of other nodes of the edge as the value, along with the original edge
   if the edge is real edge (short edge) then from and to nodes are original from and to within G.Edge
-}


type alias Adjacency =
    IntDict (List OtherNodeOfEdge)


type alias Layer =
    { nodes : List G.NodeId
    , incomingEdges : Adjacency -- key is to node, value is list of from nodes (to node belongs to this layer)
    , outgoingEdges : Adjacency -- key is from node, value is list of to nodes (from node belongs to this layer)
    }


type alias OldLayer =
    List G.NodeId


type alias RankedLayers =
    IntDict Layer


type alias Order =
    Int


type alias NodeOrderDict =
    IntDict Order


type alias OrderedEdge =
    ( Order, Order )


type alias NodeWithHeuristicValue =
    { id : G.NodeId, heuristicValue : Maybe Float }



{-
   Calculates the heuristic value for a node based on its adjacent nodes and current nodeOrderDict
-}


type alias Heuristic =
    { id : G.NodeId
    , adjacentNodes : List G.NodeId
    , nodeOrderDict : NodeOrderDict
    }
    -> NodeWithHeuristicValue


type EdgeType
    = Inner
    | NonInner


type alias EdgeWithType =
    ( Edge, EdgeType )


type alias NeighbourFn =
    G.NodeId -> List G.NodeId


type alias RankedNodeInfo =
    { rank : Int
    , priority : Maybe Int
    }



{- This type stores the ranked information about original from and to nodes of an edge
   if the edge is real edge (short edge) then from and to nodes are original from and to withing G.Edge
   if the edge is a long edge, then from and to nodes are the nodes of the original edge
-}


type alias OriginalEdgeInfo =
    { from : G.Node RankedNodeInfo
    , to : G.Node RankedNodeInfo
    }



{- This represents a graph with nodes having rank information
   along with priority and edges information
   This is used for partitioning the graph into connected components for minimizing crossings
   This graph also helps in generating initial orderings of nodes in each layer using BFS
-}


type alias RankedGraph =
    G.Graph RankedNodeInfo OriginalEdgeInfo


type alias ConnectedComponent =
    { roots :
        -- root nodes of the connected component
        { noIncomingEdges : List (G.Node RankedNodeInfo) -- List of nodes with no incoming edges
        , noOutgoingEdges : List (G.Node RankedNodeInfo) -- List of nodes with no outgoing edges
        }
    , nodes : List G.NodeId -- List of all nodes in the connected component (including root nodes)
    , size : Int -- Number of nodes in the connected component
    , rankedLayers : RankedLayers
    }


toEdge : G.Edge e -> Edge
toEdge e =
    ( e.from, e.to )


fromIncoming : G.Adjacency e -> G.NodeId -> List Edge
fromIncoming adjDict to =
    IntDict.keys adjDict
        |> List.map (\from -> ( from, to ))


fromOutgoing : G.Adjacency e -> G.NodeId -> List Edge
fromOutgoing adjDict from =
    IntDict.keys adjDict
        |> List.map (\to -> ( from, to ))


getEdges : G.Graph n e -> List Edge
getEdges g =
    let
        edges =
            G.edges g
    in
    List.map toEdge edges


alongOutgoingEdges : List Edge -> G.NodeId -> List G.NodeId
alongOutgoingEdges edges nodeId =
    List.filter (\e -> Tuple.first e == nodeId) edges
        |> List.map (\e -> Tuple.second e)


alongIncomingEdges : List Edge -> G.NodeId -> List G.NodeId
alongIncomingEdges edges nodeId =
    List.filter (\e -> Tuple.second e == nodeId) edges
        |> List.map (\e -> Tuple.first e)


getInEdges : G.NodeId -> List EdgeWithType -> List EdgeWithType
getInEdges nodeId edges =
    List.filter (\e -> (Tuple.first e |> Tuple.second) == nodeId) edges


getNodeRankDict : RankedLayers -> IntDict Int
getNodeRankDict rankedLayers =
    IntDict.toList rankedLayers
        |> List.map (\( rank, layer ) -> List.map (\node -> ( node, rank )) layer.nodes)
        |> List.concat
        |> IntDict.fromList


getRank : G.NodeId -> IntDict Int -> Int
getRank nodeId nodeRankDict =
    case IntDict.get nodeId nodeRankDict of
        Just x ->
            x

        Nothing ->
            -1


getEdgesFromPath : List G.NodeId -> List Edge
getEdgesFromPath path =
    let
        froms =
            List.take (List.length path - 1) path

        tos =
            List.drop 1 path
    in
    List.map2 (\from to -> ( from, to )) froms tos



{-
   This function returns the index of a node in a layer,
   if the node does not exist then it returns -1
-}


getOrder : NodeOrderDict -> G.NodeId -> Int
getOrder nodeOrderDict nodeId =
    case IntDict.get nodeId nodeOrderDict of
        Just idx ->
            idx

        Nothing ->
            -1


syncRankedLayersWithNodeOrderDict : RankedLayers -> NodeOrderDict -> RankedLayers
syncRankedLayersWithNodeOrderDict rankedLayers nodeOrderDict =
    IntDict.map
        (\_ layer -> { layer | nodes = List.sortBy (\node -> getOrder nodeOrderDict node) layer.nodes })
        rankedLayers


getOrderOldLayer : OldLayer -> G.NodeId -> Int
getOrderOldLayer l nodeId =
    case LE.elemIndex nodeId l of
        Just idx ->
            idx

        Nothing ->
            -1


mapEdgeToOrder : NodeOrderDict -> Edge -> OrderedEdge
mapEdgeToOrder nodeOrderDict e =
    Tuple.mapBoth (getOrder nodeOrderDict) (getOrder nodeOrderDict) e


mapEdgeToOrderOldLayer : ( OldLayer, OldLayer ) -> Edge -> Edge
mapEdgeToOrderOldLayer ( l1, l2 ) e =
    Tuple.mapBoth (getOrderOldLayer l1) (getOrderOldLayer l2) e


mapEdgeWithTypeToOrder : ( OldLayer, OldLayer ) -> EdgeWithType -> EdgeWithType
mapEdgeWithTypeToOrder ( l1, l2 ) e =
    Tuple.mapFirst (mapEdgeToOrderOldLayer ( l1, l2 )) e


getNodeFromOrder : OldLayer -> Int -> G.NodeId
getNodeFromOrder l order =
    case LE.getAt order l of
        Just n ->
            n

        Nothing ->
            intMin


mapEdgeOrderToNode : ( OldLayer, OldLayer ) -> Edge -> Edge
mapEdgeOrderToNode ( l1, l2 ) e =
    Tuple.mapBoth (getNodeFromOrder l1) (getNodeFromOrder l2) e


mapEdgeWithTypeToNodes : ( OldLayer, OldLayer ) -> EdgeWithType -> EdgeWithType
mapEdgeWithTypeToNodes ( l1, l2 ) e =
    Tuple.mapFirst (mapEdgeOrderToNode ( l1, l2 )) e


getEdgesDirectedFromLayers : ( Layer, Layer ) -> List Edge
getEdgesDirectedFromLayers ( l1, l2 ) =
    allOutGoingEdges l1


getEdgesWithTypeDirectedFromLayers : ( OldLayer, OldLayer ) -> List EdgeWithType -> List EdgeWithType
getEdgesWithTypeDirectedFromLayers ( l1, l2 ) edges =
    List.filter (\( ( from, to ), _ ) -> List.member from l1 && List.member to l2) edges


getAdjacentLayerPairs : List OldLayer -> List ( OldLayer, OldLayer )
getAdjacentLayerPairs rankList =
    let
        fromLayers =
            List.take (List.length rankList - 1) rankList

        toLayers =
            List.drop 1 rankList
    in
    List.map2 (\l1 l2 -> ( l1, l2 )) fromLayers toLayers


getLayer : Int -> RankedLayers -> Layer
getLayer rank rankedLayers =
    let
        layer =
            IntDict.get rank rankedLayers
    in
    Maybe.withDefault
        { nodes = []
        , incomingEdges = IntDict.empty
        , outgoingEdges = IntDict.empty
        }
        layer


allOutGoingEdges : Layer -> List Edge
allOutGoingEdges layer =
    IntDict.toList layer.outgoingEdges
        |> List.map (\( from, tos ) -> List.map (\{ otherNode } -> ( from, otherNode )) tos)
        |> List.concat


allInComingEdges : Layer -> List Edge
allInComingEdges layer =
    IntDict.toList layer.incomingEdges
        |> List.map (\( to, froms ) -> List.map (\{ otherNode } -> ( otherNode, to )) froms)
        |> List.concat



{- This function turns the rankedLayers into a graph with ranked information
   This is used for partitioning the graph into connected components for minimizing crossings
   This graph also helps in generating initial orderings of nodes in each layer using BFS
-}


rankedLayersToGraph : RankedLayers -> RankedGraph
rankedLayersToGraph rankedLayers =
    let
        nodes =
            IntDict.toList rankedLayers
                |> List.concatMap
                    (\( rank, layer ) ->
                        List.map
                            (\nodeId ->
                                G.Node nodeId
                                    { rank = rank
                                    , priority = Nothing
                                    }
                            )
                            layer.nodes
                    )

        allOutGoingEdgesWithRank : Layer -> List ( G.NodeId, OtherNodeOfEdge )
        allOutGoingEdgesWithRank =
            \layer ->
                IntDict.toList layer.outgoingEdges
                    |> List.map (\( from, tos ) -> List.map (\otherNodeOfEdge -> ( from, otherNodeOfEdge )) tos)
                    |> List.concat

        mapRankedNodeToRankedNodeInfoNode : RankedNode -> G.Node RankedNodeInfo
        mapRankedNodeToRankedNodeInfoNode =
            \node ->
                G.Node node.id
                    { rank = node.rank
                    , priority = Nothing
                    }

        edges : List (G.Edge OriginalEdgeInfo)
        edges =
            IntDict.values rankedLayers
                |> List.concatMap
                    (\layer ->
                        List.map
                            (\( from, to ) ->
                                G.Edge from
                                    to.otherNode
                                    (OriginalEdgeInfo
                                        (mapRankedNodeToRankedNodeInfoNode to.edge.from)
                                        (mapRankedNodeToRankedNodeInfoNode to.edge.to)
                                    )
                            )
                            (allOutGoingEdgesWithRank layer)
                    )
    in
    G.fromNodesAndEdges
        nodes
        edges


isDummyNode : G.NodeId -> G.NodeId -> Bool
isDummyNode initDummyId nodeId =
    if nodeId < initDummyId then
        False

    else
        True


getEdgeType : EdgeWithType -> EdgeType
getEdgeType edge =
    Tuple.second edge


markEdgeWithEdgeType : Int -> Edge -> EdgeWithType
markEdgeWithEdgeType initDummyId e =
    let
        ( from, to ) =
            e
    in
    if (from >= initDummyId) && (to >= initDummyId) then
        ( e, Inner )

    else
        ( e, NonInner )


markEdgesWithEdgeType : G.Graph n e -> List Edge -> List EdgeWithType
markEdgesWithEdgeType g edges =
    let
        initDummyId =
            case List.map (\n -> n.id) (G.nodes g) |> List.maximum of
                Just x ->
                    x + 1

                Nothing ->
                    -1
    in
    List.map (markEdgeWithEdgeType initDummyId) edges


filterEdgesByType : EdgeType -> List EdgeWithType -> List Edge
filterEdgesByType eType edges =
    List.filter (\e -> Tuple.second e == eType) edges
        |> List.map (\fe -> Tuple.first fe)
