module Dagre.Order.Heuristic exposing (HeuristicAccumulator, NodeOrderWithCrossCount, optimizeOrderingViaHeuristic)

import Dagre.Order.CrossCount as DOC
import Dagre.Order.Heuristic.Barycenter as DOHB
import Dagre.Order.Heuristic.Transpose as DOHT
import Dagre.Utils as DU
import Dict
import Graph
import IntDict



{-
   The main loop that minimizes the crossing reductions
-}


optimizeOrderingViaHeuristic : DU.RankedLayers -> ( Int, Int, Int ) -> HeuristicAccumulator -> HeuristicAccumulator
optimizeOrderingViaHeuristic layering ( iter, maxIter, trying ) heuristicAccumulator =
    if iter < maxIter && trying < minQuit && heuristicAccumulator.best.crossCount /= 0 then
        let
            newNodeOrderDict =
                sweepLayers layering iter heuristicAccumulator.current.nodeOrderDict

            current =
                { crossCount = DOC.crossCount layering newNodeOrderDict
                , nodeOrderDict = newNodeOrderDict
                }
        in
        if current.crossCount <= heuristicAccumulator.best.crossCount then
            let
                newTrying =
                    if toFloat current.crossCount < convergence * toFloat heuristicAccumulator.best.crossCount then
                        0

                    else
                        trying + 1
            in
            optimizeOrderingViaHeuristic layering ( iter + 1, maxIter, newTrying ) { current = current, best = current }

        else
            optimizeOrderingViaHeuristic layering ( iter + 1, maxIter, trying + 1 ) { heuristicAccumulator | current = current }

    else
        heuristicAccumulator



{-
   sorts Layers from top to bottom or bottom to top based on heuristic values
   then apply the transpose heuristic to greedily reduce the edge crossings

-}
-- equivalent to mincross_step in GraphViz


sweepLayers : DU.RankedLayers -> Int -> DU.NodeOrderDict -> DU.NodeOrderDict
sweepLayers rankedLayers iter nodeOrderDict =
    let
        reverse =
            modBy 4 iter < 2

        newOrderDict =
            if modBy 2 iter == 0 then
                let
                    rankedLayersWithoutMinRank =
                        case IntDict.findMin rankedLayers of
                            Just ( minRank, _ ) ->
                                IntDict.remove minRank rankedLayers

                            Nothing ->
                                rankedLayers
                in
                -- Applies BarryCenter Heuristic from layer 1 (0 based index) to Last layer
                IntDict.foldl (always (apply DOHB.barycenter PreviousLayer reverse)) nodeOrderDict rankedLayersWithoutMinRank

            else
                let
                    rankedLayersWithoutMaxRank =
                        case IntDict.findMax rankedLayers of
                            Just ( maxRank, _ ) ->
                                IntDict.remove maxRank rankedLayers

                            Nothing ->
                                rankedLayers
                in
                -- Applies BarryCenter Heuristic from 2nd last layer to 0th layer
                IntDict.foldr (always (apply DOHB.barycenter NextLayer reverse)) nodeOrderDict rankedLayersWithoutMaxRank
    in
    DOHT.transpose rankedLayers (not reverse) newOrderDict



{-
   This is the helper function, which uses a heuristic function to minimize crossings
    -- calculate heuristic for all nodes of layering[rank] as List {id : G.NodeId, heuristicValue : Float}
    -- sort the List based on heuristicValue
    -- update the order in the nodeOrderDict with above sorted list.
    -- TODO : [X] Implement Barycenter Heuristic
              [ ] Implement Weighted Median Heuristic from GraphViz

-}


apply : DU.Heuristic -> FixedLayer -> Bool -> DU.Layer -> DU.NodeOrderDict -> DU.NodeOrderDict
apply heuristic fixedLayer reverse layer nodeOrderDict =
    let
        getAdjacentNodes : Graph.NodeId -> DU.Adjacency -> List Graph.NodeId
        getAdjacentNodes nodeId adjacency =
            IntDict.get nodeId adjacency
                |> Maybe.withDefault []
                |> List.map .otherNode

        heuristicValues =
            case fixedLayer of
                PreviousLayer ->
                    List.map
                        (\nodeId ->
                            heuristic
                                { id = nodeId
                                , adjacentNodes = getAdjacentNodes nodeId layer.incomingEdges
                                , nodeOrderDict = nodeOrderDict
                                }
                        )
                        layer.nodes

                NextLayer ->
                    List.map
                        (\nodeId ->
                            heuristic
                                { id = nodeId
                                , adjacentNodes = getAdjacentNodes nodeId layer.outgoingEdges
                                , nodeOrderDict = nodeOrderDict
                                }
                        )
                        layer.nodes

        needReordering =
            List.filterMap
                (\{ id, heuristicValue } ->
                    case heuristicValue of
                        Just value ->
                            Just { id = id, heuristicValue = value, previousOrder = DU.getOrder nodeOrderDict id }

                        Nothing ->
                            Nothing
                )
                heuristicValues

        availableOrders =
            List.map (\{ id } -> DU.getOrder nodeOrderDict id) needReordering
                |> List.sort

        newOrder =
            sortByHeuristicValue layer.nodesCount reverse needReordering
                |> List.map2 (\order id -> ( id, order )) availableOrders
    in
    List.foldl (\( nodeId, order ) nodeOrdDict -> IntDict.insert nodeId order nodeOrdDict) nodeOrderDict newOrder



{- Sort the affected nodes based on heuristic values
   if heuristic values are same then switch only if a appears before b in the current order and reverse is true

   Note : This optimization is taken from GraphViz, although it is not exactly same, but achieves the same result
          as the original algorithm. It first sorts the heuristic values and if nodes have same heuristic values
          then it sorts them based on the previous order of the nodes. If reverse is true then it shifts the nodes
            by (total_nodes_in_layer mod count_of_nodes_with_same_heuristic_value) to the left

-}


sortByHeuristicValue : Int -> Bool -> List NodeAndHeuristicValue -> List Graph.NodeId
sortByHeuristicValue nodeCount reverse heuristicValues =
    let
        dict =
            List.foldl
                (\nodeAndHeuristic sortDict ->
                    if Dict.member nodeAndHeuristic.heuristicValue sortDict then
                        Dict.update nodeAndHeuristic.heuristicValue (\nodes -> Maybe.map (\( n, size ) -> ( n ++ [ nodeAndHeuristic ], size + 1 )) nodes) sortDict

                    else
                        Dict.insert nodeAndHeuristic.heuristicValue ( [ nodeAndHeuristic ], 1 ) sortDict
                )
                Dict.empty
                heuristicValues

        dictSorted =
            Dict.map
                (\_ ( v, s ) ->
                    let
                        sorted =
                            List.sortBy .previousOrder v
                    in
                    if reverse then
                        let
                            shiftCount =
                                modBy s nodeCount
                        in
                        List.drop shiftCount sorted ++ List.take shiftCount sorted

                    else
                        sorted
                )
                dict
                |> Dict.toList
                |> List.sortBy Tuple.first
                |> List.map Tuple.second
                |> List.concat
    in
    List.map .id dictSorted



{- This is a record that holds the current and best nodeOrderDictWithCrossCount
   current is the current node orderings after applying the heuristic
   best is the best node orderings before applying the heuristic
-}


type alias HeuristicAccumulator =
    { current : NodeOrderWithCrossCount
    , best : NodeOrderWithCrossCount
    }



{- This is a record that holds the crossCount and nodeOrderDict
   crossCount is the number of edge crossings in the graph
   nodeOrderDict is the dictionary that holds the order of nodes in each layer
-}


type alias NodeOrderWithCrossCount =
    { crossCount : Int
    , nodeOrderDict : DU.NodeOrderDict
    }


type alias NodeAndHeuristicValue =
    { id : Graph.NodeId, heuristicValue : Float, previousOrder : Int }



{-
   Fixed Layer denote which layer is fixed corresponding to current movable layer rank
   for example if we have 3 consecutive ranks i-1, i, i+1
   considering i to be movable layer's rank
   then if fixed layer is i-1 then it is denoted by PreviousLayer
   else if fixed layer is i+1 then it is denoted by NextLayer
-}


type FixedLayer
    = PreviousLayer
    | NextLayer


minQuit : Int
minQuit =
    8


convergence : Float
convergence =
    0.995
