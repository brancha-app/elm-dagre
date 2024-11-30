module Dagre.Order.Heuristic.Barycenter exposing (barycenter)

import Dagre.Utils as DU



{-
   helper function for calculating barycenter value for a node
   What is barycenter?
    - Just like median heuristic it assumes the average position
      of neighbours in fixed layer
    - For example if a node in movable layer has 3 neighbours with
      positions [1,2,5], then
        barycenter value    = Sum of positions/ Number of neighbours
                            = (1+2+5) / 3
                            = 2.67

-}


barycenter : DU.Heuristic
barycenter { id, adjacentNodes, nodeOrderDict } =
    let
        adj_positions =
            List.map (DU.getOrder nodeOrderDict) adjacentNodes
    in
    if List.isEmpty adj_positions then
        { id = id, heuristicValue = Nothing }

    else
        { id = id, heuristicValue = Just <| toFloat (List.sum adj_positions) / toFloat (List.length adj_positions) }