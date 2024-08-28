module Dagre.Order.Heuristic exposing (Best, optimizeOrderingViaHeuristic)

import Dagre.Order.CrossCount as DOC
import Dagre.Order.Heuristic.Barycenter as DOHB
import Dagre.Order.Heuristic.Transpose as DOHT
import Dagre.Utils as DU
import Graph
import IntDict



{-
   The main loop that minimizes the crossing reductions
-}


optimizeOrderingViaHeuristic : DU.RankedLayers -> ( Int, Int, Int ) -> Best -> DU.NodeOrderDict -> DU.NodeOrderDict
optimizeOrderingViaHeuristic layering ( iter, maxIter, trying ) best nodeOrderDict =
    if iter < maxIter && trying < minQuit && best.crossCount /= 0 then
        let
            newNodeOrderDict =
                sweepLayers layering iter nodeOrderDict

            newCC =
                DOC.crossCount layering newNodeOrderDict
        in
        if newCC < best.crossCount then
            let
                newTrying =
                    if toFloat newCC < convergence * toFloat best.crossCount then
                        0

                    else
                        trying + 1
            in
            optimizeOrderingViaHeuristic layering ( iter + 1, maxIter, newTrying ) { crossCount = newCC, nodeOrderDict = newNodeOrderDict } newNodeOrderDict

        else
            optimizeOrderingViaHeuristic layering ( iter + 1, maxIter, trying + 1 ) best newNodeOrderDict

    else if best.crossCount > 0 then
        DOHT.transpose layering False best.nodeOrderDict

    else
        best.nodeOrderDict



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
            IntDict.get nodeId adjacency |> Maybe.withDefault []

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
                            Just { id = id, heuristicValue = value }

                        Nothing ->
                            Nothing
                )
                heuristicValues

        availableOrders =
            List.map (\{ id } -> DU.getOrder nodeOrderDict id) needReordering
                |> List.sort

        newOrder =
            List.sortWith (compareHeuristicValues nodeOrderDict reverse) needReordering
                |> List.map2 (\order { id } -> ( id, order )) availableOrders
    in
    List.foldl (\( nodeId, order ) nodeOrdDict -> IntDict.insert nodeId order nodeOrdDict) nodeOrderDict newOrder



{- Compare the heuristic values of two nodes
   if heuristic values are same then switch only if a appears before b in the current order and reverse is true

   Note : This optimization is taken from GraphViz, although it is not exactly same, as graphviz uses bubble sort to sort the nodes
   based on heuristic values, but it is not implemented here, and uses the List.sortWith function to sort the nodes based on heuristic values
   GraphViz orders Adjacent nodes repeatedly like in bubblesort, and swaps the nodes with same heuristic values only if reverse is true

-}


compareHeuristicValues : DU.NodeOrderDict -> Bool -> NodeAndHeuristicValue -> NodeAndHeuristicValue -> Order
compareHeuristicValues nodeOrderDict reverse a b =
    case compare a.heuristicValue b.heuristicValue of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            case compare (DU.getOrder nodeOrderDict a.id) (DU.getOrder nodeOrderDict b.id) of
                LT ->
                    if reverse then
                        GT

                    else
                        LT

                _ ->
                    EQ



{- Best is a record that holds the best crossCount and nodeOrderDict
   crossCount is the number of edge crossings in the graph
   nodeOrderDict is the dictionary that holds the order of nodes in each layer
-}


type alias Best =
    { crossCount : Int
    , nodeOrderDict : DU.NodeOrderDict
    }


type alias NodeAndHeuristicValue =
    { id : Graph.NodeId, heuristicValue : Float }



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
