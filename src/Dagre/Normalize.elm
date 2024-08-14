module Dagre.Normalize exposing (addDummyNodesAndSplitEdges)

import Dagre.Utils as DU
import Dict exposing (Dict)
import Graph as G
import IntDict exposing (IntDict)



{-
   This function adds dummy nodes and splits the edges
   The edges which span over multiple layers are split.
   The split points are called Control points for the edge
   and are added as dummy nodes
    -- example
    -- input : (Layering, edges)
    -- ([[4,0],[1],[2]], [(0,1), (1,2), (4,2)])
    -- output
    -- (([[4,0],[1,5],[2]], [(0,1), (1,2), (4,5), (5,2)]), (4,2) = [5])

-}


addDummyNodesAndSplitEdges : Maybe G.NodeId -> ( DU.RankedLayers, List DU.Edge ) -> ( ( DU.RankedLayers, List DU.Edge ), Dict DU.Edge (List G.NodeId) )
addDummyNodesAndSplitEdges maybeInitDummyNodeId ( rankedLayers, edges ) =
    let
        initDummyId =
            case maybeInitDummyNodeId of
                Just x ->
                    x

                Nothing ->
                    case IntDict.values rankedLayers |> List.map .nodes |> List.concat |> List.maximum of
                        Just x ->
                            x + 1

                        Nothing ->
                            1

        nodeRankDict =
            DU.getNodeRankDict rankedLayers

        initControlPoints =
            Dict.fromList <| List.map (\e -> ( e, [] )) edges

        ( newRankLayers, _, newControlPoints ) =
            List.foldl
                (checkAndSplitMultiSpanEdge nodeRankDict)
                ( rankedLayers, initDummyId, initControlPoints )
                edges

        newEdges =
            List.concat (IntDict.values newRankLayers |> List.map .outgoingEdges)
    in
    ( ( newRankLayers, newEdges ), newControlPoints )



{-
   The following functions are used for adding Dummy Nodes and Edges
-}
{-
   This function updates the whole rankLayers,Edges and Control points
   TODO : This function can have a potential bug, i.e. if both fromRank and toRank have
   negetive rankValues. Need to Deal with that.

-}


checkAndSplitMultiSpanEdge : IntDict Int -> DU.Edge -> ( DU.RankedLayers, G.NodeId, Dict DU.Edge (List G.NodeId) ) -> ( DU.RankedLayers, G.NodeId, Dict DU.Edge (List G.NodeId) )
checkAndSplitMultiSpanEdge nodeRankDict ( from, to ) ( rankedLayers, dummyId, controlPoints ) =
    let
        ( fromRank, toRank ) =
            ( DU.getRank from nodeRankDict, DU.getRank to nodeRankDict )
    in
    if toRank - fromRank > 1 then
        let
            newDummyId =
                dummyId + toRank - fromRank - 1

            dummyNodes =
                List.range dummyId (newDummyId - 1)

            newRankedLayers =
                splitEdgeAndUpdateRankedLayers ( from, to ) ( fromRank, toRank ) dummyNodes rankedLayers

            newControlPoints =
                Dict.update ( from, to ) (Maybe.map (\_ -> dummyNodes)) controlPoints
        in
        ( newRankedLayers, newDummyId, newControlPoints )

    else
        ( rankedLayers, dummyId, controlPoints )



{-
   This function updates edges and add the splitted edges in to list.
-}


splitEdgeAndUpdateRankedLayers : DU.Edge -> ( Int, Int ) -> List G.NodeId -> DU.RankedLayers -> DU.RankedLayers
splitEdgeAndUpdateRankedLayers ( from, to ) ( fromRank, toRank ) dummyNodes rankedLayers =
    let
        updateFromLayer =
            IntDict.update fromRank (Maybe.map (\layer -> { layer | outgoingEdges = List.filter ((/=) ( from, to )) layer.outgoingEdges })) rankedLayers

        updateToLayer =
            IntDict.update toRank (Maybe.map (\layer -> { layer | incomingEdges = List.filter ((/=) ( from, to )) layer.incomingEdges })) updateFromLayer

        ( fromNodes, currentNodes, toNodes ) =
            let
                justDummyNodes =
                    List.map Just dummyNodes
            in
            ( List.concat [ [ Nothing ], [ Just from ], justDummyNodes ]
            , List.concat [ [ from ], dummyNodes, [ to ] ]
            , List.concat [ justDummyNodes, [ Just to ], [ Nothing ] ]
            )

        affectedRanks =
            List.range fromRank toRank

        addToLayers =
            List.map4 modifyLayer fromNodes currentNodes toNodes affectedRanks
    in
    List.foldl (\( rank, addToLayer ) layers -> IntDict.update rank (Maybe.map (insertKNodesIntoKSubsequentLayersWithEdges addToLayer)) layers) updateToLayer addToLayers



{-
   The following function inserts K nodes into K subsequent Layers
   For example [[5,0], [1], [4,3,2], [7,6], [8]] is the initial layer and
   we want to insert [9,10] from rank 1 onwards, it will give
   [[5,0], [1,9], [4,3,2,10], [7,6], [8]]
-}


insertKNodesIntoKSubsequentLayersWithEdges : AddToLayer -> DU.Layer -> DU.Layer
insertKNodesIntoKSubsequentLayersWithEdges addToLayer layer =
    let
        newNodes =
            case addToLayer.node of
                Just x ->
                    List.append layer.nodes [ x ]

                Nothing ->
                    layer.nodes

        newIncomingEdges =
            case addToLayer.incoming of
                Just incomingEdge ->
                    List.append layer.incomingEdges [ incomingEdge ]

                Nothing ->
                    layer.incomingEdges

        newOutgoingEdges =
            case addToLayer.outgoing of
                Just outgoingEdge ->
                    List.append layer.outgoingEdges [ outgoingEdge ]

                Nothing ->
                    layer.outgoingEdges
    in
    { layer | nodes = newNodes, incomingEdges = newIncomingEdges, outgoingEdges = newOutgoingEdges }


type alias AddToLayer =
    { node : Maybe G.NodeId
    , incoming : Maybe DU.Edge
    , outgoing : Maybe DU.Edge
    }


modifyLayer : Maybe G.NodeId -> G.NodeId -> Maybe G.NodeId -> Int -> ( Int, AddToLayer )
modifyLayer maybeFromNode currentNode maybeToNode rank =
    ( rank
    , { node = Maybe.andThen (\_ -> Maybe.andThen (\_ -> Just currentNode) maybeToNode) maybeFromNode
      , incoming = Maybe.andThen (\fromNode -> Just ( fromNode, currentNode )) maybeFromNode
      , outgoing = Maybe.andThen (\toNode -> Just ( currentNode, toNode )) maybeToNode
      }
    )
