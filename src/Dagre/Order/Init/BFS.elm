module Dagre.Order.Init.BFS exposing (BFSDirection(..), buildRanks)

import Dagre.Order.Heuristic.Transpose as DOHT
import Dagre.Utils as DU
import Graph
import IntDict



{- Compare two nodes based on their rank and id.
   closer rank and lower id is preferred.
   TODO : Integrate the priority based ordering
   Note: Rank and Id are of the original edges and not the chained edges for edges broken by dummy nodes
-}


compareNodeInfo : BFSDirection -> Graph.Node DU.RankedNodeInfo -> Graph.Node DU.RankedNodeInfo -> Order
compareNodeInfo along node1 node2 =
    if node1.label.rank == node2.label.rank then
        Basics.compare node1.id node2.id

    else
        case along of
            AlongOutgoing ->
                Basics.compare node1.label.rank node2.label.rank

            AlongIncoming ->
                -- reverse the order for rank comparison
                Basics.compare node2.label.rank node1.label.rank



{- Helper function to order the seeds based on the direction -}


getSeeds : BFSDirection -> DU.ConnectedComponent -> List Graph.NodeId
getSeeds along graph =
    let
        eligibleNodes =
            case along of
                AlongOutgoing ->
                    graph.roots.noIncomingEdges

                AlongIncoming ->
                    graph.roots.noOutgoingEdges

        seeds =
            List.sortWith (compareNodeInfo along) eligibleNodes
    in
    List.map .id seeds



{- Helper function to select the neighbour nodes based on the direction
   and use the original edge info for comparison
-}


selectNeighbourNodes : BFSDirection -> Graph.NodeContext DU.RankedNodeInfo DU.OriginalEdgeInfo -> List Graph.NodeId
selectNeighbourNodes along ctx =
    case along of
        AlongOutgoing ->
            IntDict.toList ctx.outgoing
                |> List.sortWith (\( _, a ) ( _, b ) -> compareNodeInfo along a.to b.to)
                |> List.map Tuple.first

        AlongIncoming ->
            IntDict.toList ctx.incoming
                |> List.sortWith (\( _, a ) ( _, b ) -> compareNodeInfo along a.from b.from)
                |> List.map Tuple.first


{-| Install nodes in ranks. the initial ordering ensure that series-parallel
graphs such as trees are drawn with no crossings. it tries searching
in- and out-edges and takes the better of the two initial orderings.
Taken from GraphViz implementation
-}
buildRanks : BFSDirection -> DU.RankedGraph -> DU.ConnectedComponent -> DU.NodeOrderDict
buildRanks bfsDirection rankedGraph connectedComponent =
    let
        seeds =
            getSeeds bfsDirection connectedComponent

        neighbourSelector =
            selectNeighbourNodes bfsDirection

        ( rankedLayersOrdered, _ ) =
            List.foldl
                (\seed ( rankedOrdersAcc, unVisitedGraph ) ->
                    Graph.guidedBfs
                        neighbourSelector
                        (Graph.ignorePath
                            (\nodeCtx rankedOrders ->
                                case IntDict.get nodeCtx.node.label.rank rankedOrders of
                                    Just nodes ->
                                        IntDict.update nodeCtx.node.label.rank (Maybe.map (always (nodes ++ [ nodeCtx.node.id ]))) rankedOrders

                                    Nothing ->
                                        IntDict.insert nodeCtx.node.label.rank [ nodeCtx.node.id ] rankedOrders
                            )
                        )
                        [ seed ]
                        rankedOrdersAcc
                        unVisitedGraph
                )
                ( IntDict.empty, rankedGraph )
                seeds

        nodeOrderDict =
            IntDict.foldl
                (\_ nodes orderDict ->
                    List.indexedMap Tuple.pair nodes
                        |> List.foldl
                            (\( order, nodeId ) nodeOrdrDict ->
                                IntDict.insert nodeId order nodeOrdrDict
                            )
                            orderDict
                )
                IntDict.empty
                rankedLayersOrdered
    in
    DOHT.transpose connectedComponent.rankedLayers False nodeOrderDict



{- Direction of BFS traversal -}


type BFSDirection
    = AlongOutgoing
    | AlongIncoming
