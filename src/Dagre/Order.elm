module Dagre.Order exposing (vertexOrder)

import Dagre.Order.ConnectedComponents as DOCC
import Dagre.Order.CrossCount as DOC
import Dagre.Order.Heuristic as DOH
import Dagre.Order.Heuristic.Transpose as DOHT
import Dagre.Order.Init as DOI
import Dagre.Order.Init.BFS as BFS
import Dagre.Utils as DU
import IntDict



{-
   This function returns the updated rankList such that, it minimizes the edge
   crossings between the layers.
   This algorithm is taken from GraphViz implementation of crossing reduction algorithm.

    This algorithm is presented in the following paper:
        A method for drawing directed graphs - dot's algorithm (1993)
        [TSE93.pdf](https://graphviz.org/documentation/TSE93.pdf)


-}


vertexOrder : DU.RankedLayers -> DU.RankedLayers
vertexOrder layering =
    let
        rankedGraph =
            DU.rankedLayersToGraph layering

        minRankNodes =
            IntDict.findMin layering
                |> Maybe.map (Tuple.second >> .nodes)
                |> Maybe.withDefault []

        connectedComponentsDict =
            DOCC.getConnectedComponents minRankNodes rankedGraph layering

        crossingMinimisedComponents =
            IntDict.map (always (minCross rankedGraph)) connectedComponentsDict
    in
    DOCC.mergeComponentsToRankedLayers crossingMinimisedComponents layering



{- This function is the main function that minimizes the edge crossings between the layers
   for a given connected component.
-}


minCross : DU.RankedGraph -> DU.ConnectedComponent -> DU.ConnectedComponent
minCross rankedGraph connectedComponent =
    let
        -- can be used to re-init orderings for DynaDag
        -- if we get a previous nodeOrderDict, then initialise the component using that
        -- and make sure to re-number orders from 0, new nodes can be handled as they are being now
        -- then run the mincross algorithm
        initNodeOrderDict =
            DOI.initOrder connectedComponent.rankedLayers

        current =
            { crossCount = DOC.crossCount connectedComponent.rankedLayers initNodeOrderDict
            , nodeOrderDict = initNodeOrderDict
            }

        heuristicAccumulator =
            { current = current
            , best = current
            }

        { best } =
            minCrossPass rankedGraph connectedComponent 0 heuristicAccumulator

        bestNodeOrderDict =
            if best.crossCount > 0 then
                DOHT.transpose connectedComponent.rankedLayers False best.nodeOrderDict

            else
                best.nodeOrderDict

        finalRankedLayers =
            DU.syncRankedLayersWithNodeOrderDict connectedComponent.rankedLayers bestNodeOrderDict
    in
    { connectedComponent
        | rankedLayers = finalRankedLayers
    }



{-
   if pass == 0
       then run BuildRanks along outgoing edges
       run heuristic withBestCC initialised with default ordering
   if pass == 1
       then run BuildRanks along incoming edges
       run heuristic withBestCC initialised with bestCC and bestNodeOrderDict and inNodeOrderBFSDict from pass 0

   if pass == 2
       just run heuristic on the bestNodeOrderDict,bestCC returned from pass 1

   if pass > 2
       return the heuristicAccumulator


-}


minCrossPass : DU.RankedGraph -> DU.ConnectedComponent -> Int -> DOH.HeuristicAccumulator -> DOH.HeuristicAccumulator
minCrossPass rankedGraph connectedComponent pass heuristicAccumulator =
    case pass of
        0 ->
            let
                buildRankNodeOrder =
                    BFS.buildRanks BFS.AlongOutgoing rankedGraph connectedComponent

                current =
                    { crossCount = DOC.crossCount connectedComponent.rankedLayers buildRankNodeOrder
                    , nodeOrderDict = buildRankNodeOrder
                    }

                initAcc =
                    { heuristicAccumulator
                        | current = current
                        , best =
                            if current.crossCount <= heuristicAccumulator.best.crossCount then
                                current

                            else
                                heuristicAccumulator.best
                    }

                optAcc =
                    DOH.optimizeOrderingViaHeuristic connectedComponent.rankedLayers ( 0, 4, 0 ) initAcc
            in
            if optAcc.best.crossCount == 0 then
                optAcc

            else
                minCrossPass rankedGraph connectedComponent 1 optAcc

        1 ->
            let
                buildRankNodeOrder =
                    BFS.buildRanks BFS.AlongIncoming rankedGraph connectedComponent

                current =
                    { crossCount = DOC.crossCount connectedComponent.rankedLayers buildRankNodeOrder
                    , nodeOrderDict = buildRankNodeOrder
                    }

                initAcc =
                    { heuristicAccumulator
                        | current = current
                        , best =
                            if current.crossCount <= heuristicAccumulator.best.crossCount then
                                current

                            else
                                heuristicAccumulator.best
                    }

                optAcc =
                    DOH.optimizeOrderingViaHeuristic connectedComponent.rankedLayers ( 0, 4, 0 ) initAcc
            in
            if optAcc.best.crossCount == 0 then
                optAcc

            else
                minCrossPass rankedGraph connectedComponent 2 optAcc

        2 ->
            let
                initAcc =
                    if heuristicAccumulator.current.crossCount > heuristicAccumulator.best.crossCount then
                        { heuristicAccumulator
                            | current = heuristicAccumulator.best
                        }

                    else
                        heuristicAccumulator

                optAcc =
                    DOH.optimizeOrderingViaHeuristic connectedComponent.rankedLayers ( 0, 24, 0 ) initAcc
            in
            optAcc

        _ ->
            heuristicAccumulator
