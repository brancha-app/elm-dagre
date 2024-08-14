module Dagre.Rank exposing (assignRanks)

import Dagre.Utils as DU
import Graph as G
import IntDict



{-
   This function assigns rank to all the Nodes of a Acyclic Graph
   The ranks are returned as IntDict of Layers.
   The nodes that have the same key in IntDict have same rank.
   The list that has lower key has lower rank
   For example
    IntDict.fromList
        [ (0,{ nodes = [5,0], incomingEdges = [],outgoingEdges = []})
        , (1,{ nodes = [1], incomingEdges = [],outgoingEdges = []})
        , (2,{ nodes = [4,3,2], incomingEdges = [],outgoingEdges = []}))
        , (3,{ nodes = [7,6], incomingEdges = [],outgoingEdges = []})
        , (4,{ nodes = [8], incomingEdges = [],outgoingEdges = []})
        ]
    here both 5,0 have same rank i.e. 0, similarly 4,3,2 (rank=2).
   But [5,0] have lower rank as compared to [4,3,2]
-}


assignRanks : G.AcyclicGraph n e -> DU.RankedLayers
assignRanks g =
    let
        heightLevels =
            G.heightLevels g

        toLayer : List (G.NodeContext n e) -> DU.Layer
        toLayer nodes =
            { nodes = List.map (.node >> .id) nodes
            , incomingEdges = List.foldl (\nodeCtx inEdges -> List.append (DU.fromIncoming nodeCtx.incoming nodeCtx.node.id) inEdges) [] nodes
            , outgoingEdges = List.foldl (\nodeCtx outEdges -> List.append (DU.fromOutgoing nodeCtx.outgoing nodeCtx.node.id) outEdges) [] nodes
            }
    in
    heightLevels
        |> List.indexedMap (\i nodes -> ( i, toLayer nodes ))
        |> IntDict.fromList
