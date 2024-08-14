module Dagre.Order.Init exposing (initOrder)

import Dagre.Utils as DU
import IntDict



{-
   Gives initial order for all layers by sorting them based on Node Ids
-}


initOrder : DU.RankedLayers -> DU.RankedLayers
initOrder rankedLayers =
    IntDict.map (\_ layer -> { layer | nodes = List.sort layer.nodes }) rankedLayers
