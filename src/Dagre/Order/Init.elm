module Dagre.Order.Init exposing (initOrder)

import Dagre.Utils as DU
import IntDict



{-
   Gives initial order for all layers by sorting them based on Node Ids
-}


initOrder : DU.RankedLayers -> DU.NodeOrderDict
initOrder rankedLayers =
    IntDict.foldl
        (\_ layer nodeOrderDict ->
            List.sort layer.nodes
                |> List.indexedMap Tuple.pair
                |> List.foldl (\( order, node ) nodeOrders -> IntDict.insert node order nodeOrders) nodeOrderDict
        )
        IntDict.empty
        rankedLayers
