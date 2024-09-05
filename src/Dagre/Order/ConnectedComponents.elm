module Dagre.Order.ConnectedComponents exposing (getConnectedComponents, mergeComponentsToRankedLayers)

import Dagre.Utils as DU
import Graph
import IntDict


alongAnyEdges : Graph.NodeContext n e -> List Graph.NodeId
alongAnyEdges nodeCtx =
    IntDict.keys nodeCtx.outgoing
        ++ IntDict.keys nodeCtx.incoming


getConnectedComponents : List Graph.NodeId -> DU.RankedGraph -> DU.RankedLayers -> IntDict.IntDict DU.ConnectedComponent
getConnectedComponents minRankNodes graph rankedLayers =
    let
        ( allConnectedComponents, _ ) =
            List.foldl
                (\nodeId ( connectedComponents, unVisitedGraph ) ->
                    let
                        ( connectedComponent, newGraph ) =
                            findComponent nodeId unVisitedGraph rankedLayers
                    in
                    ( connectedComponent :: connectedComponents, newGraph )
                )
                ( [], graph )
                minRankNodes

        connectedComponentsDict =
            List.filter (\connectedComponent -> connectedComponent.size > 1) allConnectedComponents
                |> List.sortBy .size
                |> List.reverse
                |> List.indexedMap (\i connectedComponent -> ( i, connectedComponent ))
                |> IntDict.fromList
    in
    connectedComponentsDict


findComponent : Graph.NodeId -> DU.RankedGraph -> DU.RankedLayers -> ( DU.ConnectedComponent, DU.RankedGraph )
findComponent seed graph unPartitionedRankedLayers =
    Graph.guidedDfs alongAnyEdges
        (Graph.onFinish
            (.node >> addNodeToConnectedComponent unPartitionedRankedLayers)
        )
        [ seed ]
        { roots = { noIncomingEdges = [], noOutgoingEdges = [] }
        , nodes = []
        , size = 0
        , rankedLayers = IntDict.empty
        }
        graph


mergeComponentsToRankedLayers : IntDict.IntDict DU.ConnectedComponent -> DU.RankedLayers -> DU.RankedLayers
mergeComponentsToRankedLayers connectedComponentsDict rankedLayers =
    let
        resetNodeOrderLayering =
            IntDict.map (\_ layer -> { layer | nodes = [] }) rankedLayers
    in
    IntDict.foldl
        (\_ connectedComponent layering ->
            appendRankedLayers connectedComponent layering
        )
        resetNodeOrderLayering
        connectedComponentsDict


appendRankedLayers : DU.ConnectedComponent -> DU.RankedLayers -> DU.RankedLayers
appendRankedLayers connectedComponent rankedLayers =
    IntDict.foldl
        (\rank layer layering ->
            IntDict.update rank (Maybe.map (\l -> { l | nodes = l.nodes ++ layer.nodes })) layering
        )
        rankedLayers
        connectedComponent.rankedLayers


addNodeToConnectedComponent : DU.RankedLayers -> Graph.Node DU.RankedNodeInfo -> DU.ConnectedComponent -> DU.ConnectedComponent
addNodeToConnectedComponent unPartitionedRankedLayers node connectedComp =
    let
        rank =
            node.label.rank

        layer =
            DU.getLayer rank connectedComp.rankedLayers

        ( incomingEdges, outgoingEdges ) =
            let
                unPartitionedLayer =
                    DU.getLayer rank unPartitionedRankedLayers
            in
            ( IntDict.get node.id unPartitionedLayer.incomingEdges |> Maybe.withDefault []
            , IntDict.get node.id unPartitionedLayer.outgoingEdges |> Maybe.withDefault []
            )

        updatedLayer =
            { layer
                | nodes = layer.nodes ++ [ node.id ]
                , incomingEdges =
                    IntDict.insert node.id incomingEdges layer.incomingEdges
                , outgoingEdges =
                    IntDict.insert node.id outgoingEdges layer.outgoingEdges
            }

        updatedRankedLayers =
            IntDict.insert rank updatedLayer connectedComp.rankedLayers

        newRoots =
            { noIncomingEdges =
                if List.isEmpty incomingEdges then
                    node :: connectedComp.roots.noIncomingEdges

                else
                    connectedComp.roots.noIncomingEdges
            , noOutgoingEdges =
                if List.isEmpty outgoingEdges then
                    node :: connectedComp.roots.noOutgoingEdges

                else
                    connectedComp.roots.noOutgoingEdges
            }
    in
    { connectedComp
        | nodes = node.id :: connectedComp.nodes
        , roots = newRoots
        , size = connectedComp.size + 1
        , rankedLayers = updatedRankedLayers
    }
