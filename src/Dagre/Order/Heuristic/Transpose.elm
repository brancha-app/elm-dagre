module Dagre.Order.Heuristic.Transpose exposing (transpose)

import Dagre.Order.CrossCount as DOC
import Dagre.Utils as DU
import Graph
import IntDict
import Set exposing (Set)



{-
   This function implements the transpose routine from crossing reduction algorithm
   In this function each adjacent pair of vertices is examined. Their
   order is switched if this reduces the number of crossings. The
   function crossing (v,w) simply counts the number of
   edge crossings if v appears to the left of w in their rank.
-}


transpose : DU.RankedLayers -> Bool -> DU.NodeOrderDict -> DU.NodeOrderDict
transpose rankedLayers reverse nodeOrderDict =
    transposeCandidates Set.empty rankedLayers reverse nodeOrderDict



{- This function transposes the candidate layers
   and returns the updated nodeOrderDict
   It uses delta to rerun the transposeCandidates, if the number of crossings are reduced
-}


transposeCandidates : Set Int -> DU.RankedLayers -> Bool -> DU.NodeOrderDict -> DU.NodeOrderDict
transposeCandidates nonCandidates rankedLayers reverse nodeOrderDict =
    let
        ( newNodeOrderDict, delta, updatedNonCandidates ) =
            IntDict.foldl (\rank layer acc -> optimizeLayer ( rank, layer ) reverse acc) ( nodeOrderDict, 0, nonCandidates ) rankedLayers
    in
    if delta > 0 then
        transposeCandidates updatedNonCandidates rankedLayers reverse newNodeOrderDict

    else
        newNodeOrderDict


optimizeLayer : ( Int, DU.Layer ) -> Bool -> ( DU.NodeOrderDict, Int, Set Int ) -> ( DU.NodeOrderDict, Int, Set Int )
optimizeLayer ( rank, layer ) reverse ( nodeOrderDict, delta, nonCandidates ) =
    if Set.member rank nonCandidates then
        ( nodeOrderDict, delta, nonCandidates )

    else
        let
            nodesSortedByOrder =
                List.map (\node -> ( node, DU.getOrder nodeOrderDict node )) layer.nodes
                    |> List.sortBy Tuple.second
                    |> List.map Tuple.first

            ( newOrder, deltaCurrentLayer ) =
                applyTranspose layer nodeOrderDict reverse nodesSortedByOrder
        in
        if deltaCurrentLayer == 0 then
            ( nodeOrderDict, delta, Set.insert rank nonCandidates )

        else
            ( List.indexedMap (\idx node -> ( node, idx )) newOrder
                |> List.foldl (\( node, order ) nodeOdrDict -> IntDict.insert node order nodeOdrDict) nodeOrderDict
            , delta + deltaCurrentLayer
            , nonCandidates
            )



{-
   The following function applies the transpose heuristic
   if it reduces the number of crossings then it
   swaps the i^th node with i+1^th node
   delta is the number of crossings reduced by this operation
-}


applyTranspose : DU.Layer -> DU.NodeOrderDict -> Bool -> List Graph.NodeId -> ( List Graph.NodeId, Int )
applyTranspose layer nodeOrderDict reverse remainingNodes =
    case remainingNodes of
        [] ->
            ( [], 0 )

        [ x ] ->
            ( [ x ], 0 )

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
                let
                    ( transposedXs, deltaXs ) =
                        applyTranspose layer nodeOrderDict reverse (v :: xs)
                in
                ( w :: transposedXs, c0 - c1 + deltaXs )

            else
                let
                    ( transposedXs, deltaXs ) =
                        applyTranspose layer nodeOrderDict reverse (w :: xs)
                in
                ( v :: transposedXs, deltaXs )
