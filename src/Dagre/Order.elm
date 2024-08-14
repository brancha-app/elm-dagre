module Dagre.Order exposing (vertexOrder)

import Dagre.Order.Barycenter as DOB
import Dagre.Order.CrossCount as DOC
import Dagre.Order.Init as DOI
import Dagre.Order.Transpose as DOT
import Dagre.Utils as DU
import IntDict



{-
   This function returns the updated rankList such that, it minimizes the edge
   crossings between the layers.
   This algorithm is taken from Gansner, et al., (1993) : "A Technique for Drawing Directed Graphs."
   For debugging and reference visit DagreJS implementation (commit with SHA-id '6355259')
   TODO : implement the  Jünger and Mutzel,
        "2-DU.Layer Straightline Crossing Minimization", It improves time complexity

-}


vertexOrder : DU.RankedLayers -> DU.RankedLayers
vertexOrder layering =
    let
        initLayering =
            DOI.initOrder layering

        bestCC =
            DOC.crossCount initLayering
    in
    optimizeOrdering initLayering bestCC ( 0, 0 )



{-
   The main loop that minimizes the crossing reductions
-}


optimizeOrdering : DU.RankedLayers -> Int -> ( Int, Int ) -> DU.RankedLayers
optimizeOrdering layering bestCC ( iter, lastBest ) =
    if lastBest < 3 then
        let
            newLayering =
                sweepLayers layering iter

            newTransposedLayering =
                DOT.transpose newLayering

            newCC =
                DOC.crossCount newTransposedLayering
        in
        if newCC < bestCC then
            optimizeOrdering newTransposedLayering newCC ( iter + 1, 0 )

        else
            optimizeOrdering layering bestCC ( iter + 1, lastBest + 1 )

    else
        layering



{-
   sorts Layers from top to bottom or bottom to top based on Barycenter values
-}


sweepLayers : DU.RankedLayers -> Int -> DU.RankedLayers
sweepLayers rankedLayers iter =
    let
        maxRank =
            (IntDict.keys rankedLayers
                |> List.maximum
                |> Maybe.withDefault 0
            )
                - 1
    in
    if modBy 2 iter == 0 then
        -- Applies BarryCenter Heuristic from layer 1 (0 based index) to Last layer
        List.foldl (DOB.barycenter DOB.PreviousLayer) rankedLayers (List.range 1 maxRank)

    else
        -- Applies BarryCenter Heuristic from 2nd last layer to 0th layer
        List.foldl (DOB.barycenter DOB.NextLayer) rankedLayers (List.range 0 (maxRank - 1) |> List.reverse)
