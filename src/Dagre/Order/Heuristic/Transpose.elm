module Dagre.Order.Heuristic.Transpose exposing (transpose)

import Dagre.Order.CrossCount as DOC
import Dagre.Utils as DU
import Graph
import IntDict



{-
   This function implements the transpose routine from crossing reduction algorithm
   In this function each adjacent pair of vertices is examined. Their
   order is switched if this reduces the number of crossings. The
   function crossing (v,w) simply counts the number of
   edge crossings if v appears to the left of w in their rank.
-}


transpose : DU.RankedLayers -> Bool -> DU.NodeOrderDict -> DU.NodeOrderDict
transpose rankedLayers reverse nodeOrderDict =
    -- todo : add untangle logic by adding delta based rerunning of transpose, make it configurable to be turned on/off by user
    IntDict.foldl (\_ layer nodeOdrDict -> optimizeLayer layer reverse nodeOdrDict) nodeOrderDict rankedLayers



{-
   The following function tries to optimize position of nodes by
   swapping adjacent nodes in the given layer.
-}


optimizeLayer : DU.Layer -> Bool -> DU.NodeOrderDict -> DU.NodeOrderDict
optimizeLayer layer reverse nodeOrderDict =
    let
        nodesSortedByOrder =
            List.map (\node -> ( node, DU.getOrder nodeOrderDict node )) layer.nodes
                |> List.sortBy Tuple.second
                |> List.map Tuple.first

        newOrder =
            applyTranspose layer nodeOrderDict reverse nodesSortedByOrder
    in
    List.indexedMap (\idx node -> ( node, idx )) newOrder
        |> List.foldl (\( node, order ) nodeOdrDict -> IntDict.insert node order nodeOdrDict) nodeOrderDict



{-
   The following function applies the transpose heuristic
   if it reduces the number of crossings then it
   swaps the i^th node with i+1^th node
-}


applyTranspose : DU.Layer -> DU.NodeOrderDict -> Bool -> List Graph.NodeId -> List Graph.NodeId
applyTranspose layer nodeOrderDict reverse remainingNodes =
    case remainingNodes of
        [] ->
            []

        [ x ] ->
            [ x ]

        v :: w :: xs ->
            let
                c0 =
                    DOC.transposeCrossCount v w layer nodeOrderDict

                c1 =
                    DOC.transposeCrossCount w v layer nodeOrderDict
            in
            if c1 < c0 || (c0 > 0 && reverse && c1 == c0) then
                -- swap v and w if it reduces the number of crossings
                -- or if the number of crossings is same and reverse is true
                w :: applyTranspose layer nodeOrderDict reverse (v :: xs)

            else
                v :: applyTranspose layer nodeOrderDict reverse (w :: xs)
