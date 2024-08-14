module Dagre.Order.Transpose exposing (transpose)

import Dagre.Order.CrossCount as DOC
import Dagre.Utils as DU
import IntDict exposing (IntDict)
import List.Extra as LE



{-
   This function implements the transpose routine from crossing reduction algorithm
   In this function each adjacent pair of vertices is examined. Their
   order is switched if this reduces the number of crossings. The
   function crossing (v,w) simply counts the number of
   edge crossings if v appears to the left of w in their rank.
-}


transpose : DU.RankedLayers -> DU.RankedLayers
transpose rankedLayers =
    let
        ( newLayering, improved ) =
            optimizeViaTranspose rankedLayers
    in
    if improved then
        transpose newLayering

    else
        rankedLayers


optimizeViaTranspose : DU.RankedLayers -> ( DU.RankedLayers, Bool )
optimizeViaTranspose layering =
    let
        maxRank =
            (IntDict.keys layering
                |> List.maximum
                |> Maybe.withDefault 0
            )
                - 1

        ranks =
            List.range 0 maxRank

        ( newLayering, improved ) =
            List.foldl optimizeLayer ( layering, False ) ranks
    in
    ( newLayering, improved )



{-
   The following function traverses through all the layer and tries to
   optimize position of nodes by swapping adjacent nodes in each layer.
-}


optimizeLayer : Int -> ( DU.RankedLayers, Bool ) -> ( DU.RankedLayers, Bool )
optimizeLayer rank ( layering, improved ) =
    let
        prevLayer =
            DU.getLayer (rank - 1) layering

        curLayer =
            DU.getLayer rank layering

        nextLayer =
            DU.getLayer (rank + 1) layering

        positions =
            List.range 0 (List.length curLayer.nodes - 2)

        ( newCurLayer, newImproved ) =
            List.foldl (optimizeNodePosition ( prevLayer, nextLayer )) ( curLayer, improved ) positions
    in
    ( IntDict.update rank (Maybe.map (\layer -> { layer | nodes = newCurLayer.nodes })) layering, newImproved )



{-
   The following function swaps the i^th node with i+1^th node and checks if
   it reduces the number of crossings
-}


optimizeNodePosition : ( DU.Layer, DU.Layer ) -> Int -> ( DU.Layer, Bool ) -> ( DU.Layer, Bool )
optimizeNodePosition ( prevLayer, nextLayer ) i ( curLayer, improved ) =
    let
        newCurLayer =
            { curLayer | nodes = LE.swapAt i (i + 1) curLayer.nodes }

        oldLayers =
            IntDict.fromList
                [ ( 0, prevLayer )
                , ( 1, curLayer )
                , ( 2, nextLayer )
                ]

        newLayers =
            IntDict.fromList
                [ ( 0, prevLayer )
                , ( 1, newCurLayer )
                , ( 2, nextLayer )
                ]
    in
    if DOC.crossCount newLayers < DOC.crossCount oldLayers then
        ( newCurLayer, True )

    else
        ( curLayer, improved )
