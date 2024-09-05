module Simple exposing (..)

import Graph as G
import Html
import Render as R
import Dagre.Attributes as DA

{- This graph is useful to illustrate the variations in final layout
   when different heuristics are used.
   With Barycenter heuristic, the graph has a few crossings.
   With WeightedMedian heuristic, the graph has no crossings.
   But this does not imply that WeightedMedian is always better than Barycenter.
-}
simpleGraph : G.Graph Int ()
simpleGraph =
    G.fromNodeLabelsAndEdgePairs
        (List.range 1 23)
        [ ( 1, 3 )
        , ( 1, 4 )
        , ( 1, 13 )
        , ( 1, 21 )
        , ( 2, 3 )
        , ( 2, 20 )
        , ( 3, 4 )
        , ( 3, 5 )
        , ( 3, 23 )
        , ( 4, 6 )
        , ( 5, 7 )
        , ( 6, 8 )
        , ( 6, 16 )
        , ( 6, 23 )
        , ( 7, 9 )
        , ( 8, 10 )
        , ( 8, 11 )
        , ( 9, 12 )
        , ( 10, 13 )
        , ( 10, 14 )
        , ( 10, 15 )
        , ( 11, 15 )
        , ( 11, 16 )
        , ( 12, 20 )
        , ( 13, 17 )
        , ( 14, 17 )
        , ( 14, 18 )
        , ( 16, 18 )
        , ( 16, 19 )
        , ( 16, 20 )
        , ( 18, 21 )
        , ( 19, 22 )
        , ( 21, 23 )
        , ( 22, 23 )
        ]


main : Html.Html msg
main =
    R.draw
        [ DA.rankDir DA.TB
        , DA.nodeSep 10
        ]
        [ R.style "height: 100vh;"
        ]
        simpleGraph
