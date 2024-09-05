module Dagre.Order.CrossCount exposing (crossCount, transposeCrossCount)

import Dagre.Utils as DU
import Graph
import IntDict
import List.Extra as LE



{-
   Comparator Lexicographical Sorting of Edges
   This function is used to sort edges based on the order of their north end, followed by south end
-}


lexSortEdge : DU.OrderedEdge -> DU.OrderedEdge -> Order
lexSortEdge ( e1NorthOrder, e1SouthOrder ) ( e2NorthOrder, e2SouthOrder ) =
    case compare e1NorthOrder e2NorthOrder of
        LT ->
            LT

        EQ ->
            compare e1SouthOrder e2SouthOrder

        GT ->
            GT



{-
   This function maps edges to position of nodes in layers
   and returns the south end of the layers sorted Lexicographically
   For example


-}


mapEdgesToSouthernSequence : DU.NodeOrderDict -> List DU.Edge -> List Int
mapEdgesToSouthernSequence nodeOrderDict edges =
    let
        orderedEdges =
            List.map (DU.mapEdgeToOrder nodeOrderDict) edges

        sortedEdges =
            List.sortWith lexSortEdge orderedEdges

        southernPoints =
            List.map (\( _, to ) -> to) sortedEdges
    in
    southernPoints



{- Swaps the element till the correct position is found -}
-- swapKey :
{-
   This function insert the pth element into correct position,
   considering all elements before p are sorted and from p are unsorted
   It also returns the number of inversion that p had
-}


insertIntoSortedWithInversions : Int -> Int -> ( Int, List Int ) -> ( Int, List Int )
insertIntoSortedWithInversions p e ( prevInversions, nodes ) =
    let
        ( sorted, unsorted ) =
            ( List.take p nodes, List.drop (p + 1) nodes )

        ( lesser, greater ) =
            ( LE.takeWhile (\n -> n <= e) sorted, LE.dropWhile (\n -> n <= e) sorted )

        finalNodes =
            List.concat [ lesser, [ e ], greater, unsorted ]
    in
    ( prevInversions + List.length greater, finalNodes )



{-
   Insertion Sort with Accumulator


-}


insertionSortWithInversionAccumulator : List Int -> ( Int, List Int )
insertionSortWithInversionAccumulator nodes =
    LE.indexedFoldl insertIntoSortedWithInversions ( 0, nodes ) nodes



{-
   BiLayer CrossCount : Returns number of crossing's between two layers.
   The functions counts the edge crossing between the given North Layer and the upcoming South Layer.
   It considers the outgoing edges from the given North Layer to the South Layer.
   The inversion count algorithm is the naive algorithm decribed in
   Barth, et al. "Simple and Efficient Bilayer Cross Counting"
   I still need to implement the efficient algorithm proposed in paper
   or search for an efficient algorithm for Pure Functional Language
   "Just a thought may be use some array from elm :
   read article dethroning the list :p"
-}


biLayerCrossCount : DU.Layer -> DU.NodeOrderDict -> Int
biLayerCrossCount northLayer nodeOrderDict =
    let
        reqEdges =
            DU.allOutGoingEdges northLayer

        reqSouthernPoints =
            mapEdgesToSouthernSequence nodeOrderDict reqEdges

        ( totalCrossings, _ ) =
            insertionSortWithInversionAccumulator reqSouthernPoints
    in
    totalCrossings



{- counts Crossing edges for a rank list -}


crossCount : DU.RankedLayers -> DU.NodeOrderDict -> Int
crossCount rankedLayers nodeOrderDict =
    let
        layeredCrossCounts =
            IntDict.map (\_ layer -> biLayerCrossCount layer nodeOrderDict) rankedLayers

        cc =
            IntDict.foldl (\_ layerCC totalCrossings -> layerCC + totalCrossings) 0 layeredCrossCounts
    in
    cc



--  Useful for Counting Crossings for applying Transpose Heuristic
{- counts crossing between two adjacent nodes efficiently -}


countCrossingsForSingleEdge : List DU.Order -> DU.Order -> Int
countCrossingsForSingleEdge preceedingEdgesOrder endPointOrder =
    List.foldl
        (\otherEdgeOrder totalCrossings ->
            if otherEdgeOrder > endPointOrder then
                totalCrossings + 1

            else
                totalCrossings
        )
        0
        preceedingEdgesOrder


countCrossingsForAllEdges : List DU.Order -> List DU.Order -> Int
countCrossingsForAllEdges preceedingEdgesOrder endPointOrders =
    List.foldl (\endPointOrder totalCrossings -> totalCrossings + countCrossingsForSingleEdge preceedingEdgesOrder endPointOrder) 0 endPointOrders



{- efficiently counts the number of crossings between two nodes, when they are adjacent

-}


transposeCrossCount : Graph.NodeId -> Graph.NodeId -> DU.Layer -> DU.NodeOrderDict -> Int
transposeCrossCount leftNode rightNode layer nodeOrderDict =
    let
        northernSequenceForIncomingEdgesSorted nodeId =
            IntDict.get nodeId layer.incomingEdges
                |> Maybe.withDefault []
                |> List.map (.otherNode >> DU.getOrder nodeOrderDict)
                |> List.sort

        southernSequenceForOutgoingEdgesSorted nodeId =
            IntDict.get nodeId layer.outgoingEdges
                |> Maybe.withDefault []
                |> List.map (.otherNode >> DU.getOrder nodeOrderDict)
                |> List.sort

        northernSeqLeftNode =
            northernSequenceForIncomingEdgesSorted leftNode

        northernSeqRightNode =
            northernSequenceForIncomingEdgesSorted rightNode

        southernSeqLeftNode =
            southernSequenceForOutgoingEdgesSorted leftNode

        southernSeqRightNode =
            southernSequenceForOutgoingEdgesSorted rightNode

        inCrossings =
            countCrossingsForAllEdges northernSeqLeftNode northernSeqRightNode

        outCrossings =
            countCrossingsForAllEdges southernSeqLeftNode southernSeqRightNode
    in
    inCrossings + outCrossings
