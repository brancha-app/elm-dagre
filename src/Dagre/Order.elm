module Dagre.Order exposing (vertexOrder)

import Dagre.Order.CrossCount as DOC
import Dagre.Order.Heuristic as DOH
import Dagre.Order.Init as DOI
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
        initNodeOrderDict =
            DOI.initOrder layering

        bestCC =
            DOC.crossCount layering initNodeOrderDict

        initBest : DOH.Best
        initBest =
            { nodeOrderDict = initNodeOrderDict
            , crossCount = bestCC
            }

        finalBestOrdering =
            DOH.optimizeOrderingViaHeuristic layering ( 0, 24, 0 ) initBest initNodeOrderDict

        finalRankedLayers =
            DU.syncRankedLayersWithNodeOrderDict layering finalBestOrdering
    in
    finalRankedLayers



-- TODO
-- 1. Initialise the ranking of nodes using BFS as in GraphViz
-- 2. Implement pass logic to apply heuristics to reduce edge crossings
--     Search for this line in lib/dotgen/mincross.c -> mincross() function 'for (pass = startpass; pass <= endpass; pass++) {'
--
--
-- {- 	install nodes in ranks. the initial ordering ensure that series-parallel
--    graphs such as trees are drawn with no crossings.  it tries searching
--    in- and out-edges and takes the better of the two initial orderings.
--    Taken from GraphViz implementation
-- -}
-- buildRanks : DU.RankedLayers -> Neighbours -> DU.NodeOrderDict
-- buildRanks layering along =
--     let
--         initNodeOrderDict =
--             DOI.initOrder layering
--     in
--     initNodeOrderDict
-- type Neighbours
--     = AlongOutgoingEdges
--     | AlongIncomingEdges
