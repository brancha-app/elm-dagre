module Dagre.Attributes exposing
    ( RankDir(..), Config, Attribute
    , rankDir, widthDict, heightDict, width, height, nodeSep, edgeSep, rankSep, marginX, marginY, initDummyNodeId
    , positionBias, heuristic, reversePriority, persistentTranspose
    , Heuristic(..), PositioningBias(..)
    )

{-| Dagre Configuration Attributes


# Types

@docs RankDir, Config, Attribute


# Attributes

These function set the respective attributes for the algorithm

**Note** :

1.  All numeric attributes can not have values < 0. If a value `v` < 0
    then absolute value of `v` is used.
2.  All numeric values are defined in pixels.

@docs rankDir, widthDict, heightDict, width, height, nodeSep, edgeSep, rankSep, marginX, marginY, initDummyNodeId


# Advanced Attributes

These attributes are used to set the advanced configuration for the algorithm
**Note** :

1.  These attributes are not recommended for general use, as they are used to set the advanced configuration for the algorithm.
2.  Use these attributes only if you are sure about the values you are setting.
3.  The default values are set to the values that are used by the algorithm, so you can safely ignore these attributes.

@docs positionBias, heuristic, reversePriority, persistentTranspose

-}

import Dict exposing (Dict)
import Graph as G


{-| This type represents the config for the dagre layout algorithm

**Note** : For complete info on default values and description of each field, please
see associated attributes.

-}
type alias Config =
    { rankDir : RankDir
    , order : Dict G.NodeId Float
    , widthDict : Dict G.NodeId Float
    , heightDict : Dict G.NodeId Float
    , width : Float
    , height : Float
    , nodeSep : Float
    , edgeSep : Float
    , rankSep : Float
    , marginX : Float
    , marginY : Float
    , initDummyNodeId : Maybe G.NodeId
    , positionBias : PositioningBias
    , heuristic : Heuristic
    , reversePriority : Bool
    , persistentTranspose : Bool
    }


{-| This type represents the rank directions.
T,L,B,R represent Top, Left, Bottom, Right respectively.
TB represent Top to Bottom direction. Similar notation is
used for other directions.
-}
type RankDir
    = TB
    | BT
    | LR
    | RL


{-| Attribute type for Dagre
-}
type alias Attribute =
    Config -> Config



-- A T T R I B U T E S --


{-| The rankDir defines the direction for rank nodes.

The possible values can be TB, BT, LR, or RL.

The default value is TB

-}
rankDir : RankDir -> Attribute
rankDir rDir =
    \a ->
        { a | rankDir = rDir }


{-| The widthDict associates nodes with a width that will be used during the layout.

    -- For example you want nodes 1,2,3 with widths 50,60,70 respectively then
    -- you can pass the following
    let
        widths =
            Dict.fromList [ ( 1, 50 ), ( 2, 60 ), ( 3, 70 ) ]
    in
    runLayout [ widthDict widths ] simplegraph

The dafualt value is Dict.empty

-}
widthDict : Dict G.NodeId Float -> Attribute
widthDict dict =
    let
        absDict =
            Dict.map (\_ v -> abs v) dict
    in
    \a ->
        { a | widthDict = absDict }


{-| The heightDict associates nodes with a height that will be used during the layout.

    -- For example you want nodes 1,2,3 with heights 30,50,40 respectively then
    -- you can pass the following
    let
        heights =
            Dict.fromList [ ( 1, 30 ), ( 2, 50 ), ( 3, 40 ) ]
    in
    runLayout [ heightDict heights ] simplegraph

The dafualt value is Dict.empty

-}
heightDict : Dict G.NodeId Float -> Attribute
heightDict dict =
    let
        absDict =
            Dict.map (\_ v -> abs v) dict
    in
    \a ->
        { a | heightDict = absDict }


{-| Defines the default width that will be used during the layout.
This value will be used when no value is available in widthDict for some node.

The default value of width is 32 pixels.

-}
width : Float -> Attribute
width w =
    \a ->
        { a | width = abs w }


{-| Defines the default height that will be used during the layout.
This value will be used when no value is available in heightDict for some node.

The default value of height is 32 pixels.

-}
height : Float -> Attribute
height h =
    \a ->
        { a | height = abs h }


{-| Defines the number of pixels that separate nodes in same rank in the layout.

The default value is 50 pixel.

-}
nodeSep : Float -> Attribute
nodeSep nSep =
    \a ->
        { a | nodeSep = abs nSep }


{-| Defines the number of pixels that separate edges horizontally in the layout.

The default value is 10 pixel.

-}
edgeSep : Float -> Attribute
edgeSep eSep =
    \a ->
        { a | edgeSep = abs eSep }


{-| Defines number of pixels between each rank in the layout.

The default value is 75 pixel.

-}
rankSep : Float -> Attribute
rankSep rSep =
    \a ->
        { a | rankSep = abs rSep }


{-| Defines the number of pixels to use as a margin around the left and right
of the graph.

The default value is 20 pixels.

-}
marginX : Float -> Attribute
marginX mX =
    \a ->
        { a | marginX = abs mX }


{-| Defines the number of pixels to use as a margin around the top and bottom
of the graph.

The default value is 20 pixel.

-}
marginY : Float -> Attribute
marginY mY =
    \a ->
        { a | marginY = abs mY }


{-| Defines the initial value for Dummy node id. These dummy nodes act as control/bend points for edges.

**Note** : You can safely assume that all the dummy nodes will have node ids greater than this value. This is attribute should only be used, when you are certain that you want to reserve node-ids till a certain range, to avoid mistakes during the drawing process.

The default value is max( G.NodeIds ) + 1 (i.e. the next available node id)

-}
initDummyNodeId : G.NodeId -> Attribute
initDummyNodeId dummyId =
    \a ->
        { a | initDummyNodeId = Just dummyId }



-- A D V A N C E D   A T T R I B U T E S --


{-| This type defines the positioning bias used for the coordinate assignment phase of the algorithm.
The values are defined for a Top to Bottom rank direction of the graph. But the values can be used for other rank directions as well.
The values are defined as follows:

  - UL : Upper Left
  - UR : Upper Right
  - DL : Down Left
  - DR : Down Right

-}
type PositioningBias
    = UL
    | UR
    | DL
    | DR
    | Balanced


{-| This type defines the crossing minimization heuristic used in Vertex Ordering Phase of the algorithm.
The values are defined as follows:

  - Barycenter
  - Median
  - WeightedMedian

**Todo** : Add the missing heuristics (Median, WeightedMedian), and their respective implementations.

-}
type Heuristic
    = Barycenter
    | Median
    | WeightedMedian


{-| This function sets the positioning bias for the coordinate assignment phase of the algorithm.
The default value is Balanced, which means the algorithm will try to balance the nodes in the graph, so that the graph looks more symmetric.
-}
positionBias : PositioningBias -> Attribute
positionBias bias =
    \a ->
        { a | positionBias = bias }


{-| This function sets the heuristic for the vertex ordering phase of the algorithm.
The default value is Barycenter, which means the barycenter heuristic will be used to minimize the edge crossings between the layers.

**Todo** : Implement the Median and WeightedMedian heuristics.

-}
heuristic : Heuristic -> Attribute
heuristic h =
    \a ->
        { a | heuristic = h }


{-| This function sets the priority for ordering nodes within the same rank.
The default value is False, which means the nodes with lower order value will be placed before the nodes with higher order value.
In case of two nodes with same order value, the nodes will be placed based on their node ids.

**Todo** : Implement the priority ordering for nodes within the same rank. This feature is not yet implemented.

-}
reversePriority : Bool -> Attribute
reversePriority rev =
    \a ->
        { a | reversePriority = rev }


{-| This function sets the persistent transpose flag for vertex ordering phase of the algorithm.
This flag is used within transpose heuristic, and will keep transposing the graph as long as the cross count is decreasing within a single pass.
The default value is True, which means the graph will be transposed until the cross count is decreasing.

**Todo** : Make the transpose heuristic respect the persistent transpose flag.

-}
persistentTranspose : Bool -> Attribute
persistentTranspose pt =
    \a ->
        { a | persistentTranspose = pt }
