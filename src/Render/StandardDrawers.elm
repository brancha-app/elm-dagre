module Render.StandardDrawers exposing (svgDrawEdge, svgDrawNode)

{-| This module provides the standard Drawers for drawing a graph. These are
default drawers for the draw function.


# Standard Drawers

@docs svgDrawEdge, svgDrawNode


## Standard Configurations

-- Todo

-}

import Color
import Curve
import Graph exposing (Node)
import Render.StandardDrawers.Attributes exposing (Attribute)
import Render.StandardDrawers.ConfigTypes exposing (..)
import Render.StandardDrawers.Types exposing (..)
import Render.Types exposing (..)
import SubPath as SP
import TypedSvg as TS exposing (g)
import TypedSvg.Attributes as TA
import TypedSvg.Core as TC exposing (Svg)
import TypedSvg.Events as TE
import TypedSvg.Types as TT


defEdgeDrawerConfig : EdgeDrawerConfig e msg
defEdgeDrawerConfig =
    let
        f =
            \e -> String.fromInt e.from ++ " → " ++ String.fromInt e.to

        f_ =
            \_ -> ""
    in
    { label = f_
    , arrowHead = None
    , onClick = Nothing
    , strokeColor = \_ -> Color.darkGrey
    , strokeWidth = \_ -> 3
    , strokeDashArray = f_
    , style = f_
    , title = f
    , linkStyle = Spline
    , alpha = 0.5
    , orientLabelAlongEdge = False
    }


defNodeDrawerConfig : NodeDrawerConfig n msg
defNodeDrawerConfig =
    let
        f =
            \n -> String.fromInt n.id

        f_ =
            \_ -> ""
    in
    { label = f
    , shape = Ellipse
    , onClick = Nothing
    , strokeColor = \_ -> Color.blue
    , strokeWidth = \_ -> 1
    , strokeDashArray = f_
    , style = f_
    , fill = \_ -> Color.rgb255 178 235 242
    , title = f
    , xLabel = f_
    , xLabelPos = \_ w h -> ( (w / 2) + 1, (-h / 2) - 1 )
    }


arrowHeadId : ArrowHeadShape -> String
arrowHeadId ah =
    case ah of
        None ->
            ""

        Triangle ->
            "url(#triangle-head)"

        Vee ->
            "url(#vee-head)"


tolerance : Float
tolerance =
    1.0


{-| Standard Edge Drawer. It can be configured using Rander.StandardDrawers.Attributes
-}
svgDrawEdge : List (Attribute (EdgeDrawerConfig e msg)) -> EdgeDrawer e msg
svgDrawEdge edits edgeAtrib =
    let
        edge =
            edgeAtrib.edge

        config =
            List.foldl (\f a -> f a) defEdgeDrawerConfig edits

        curve =
            let
                pts =
                    getAdjustedSrcAndTarget edgeAtrib 1.5 1.5
            in
            case config.linkStyle of
                Spline ->
                    Curve.catmullRom config.alpha pts

                Polyline ->
                    Curve.linear pts

        parameterizedCurve =
            SP.arcLengthParameterized tolerance curve

        edgeId =
            "edge-" ++ String.fromInt edge.from ++ "-" ++ String.fromInt edge.to

        edgePathId =
            edgeId ++ "-path"

        gAtrib =
            case config.onClick of
                Nothing ->
                    [ TA.id edgeId
                    , TA.class [ "edge" ]
                    , TA.style <| config.style edge
                    ]

                Just f ->
                    [ TA.id edgeId
                    , TA.class [ "edge" ]
                    , TA.style <| config.style edge
                    , TE.onClick (f edge)
                    , TA.cursor TT.CursorPointer
                    ]
    in
    g
        gAtrib
        [ arrowHeadDef config.arrowHead (config.strokeColor edge)
        , TS.title [] [ TC.text <| config.title edge ]
        , TS.path
            [ TA.id edgePathId
            , TA.d <| SP.toString curve
            , TA.stroke <| TT.Paint <| config.strokeColor edge
            , TA.strokeWidth <| TT.Px <| config.strokeWidth edge
            , TA.strokeDasharray <| config.strokeDashArray edge
            , TA.fill TT.PaintNone
            , TA.markerEnd (arrowHeadId config.arrowHead)
            ]
            []
        , edgeLabelDrawer (config.label edge) config.orientLabelAlongEdge edgePathId parameterizedCurve
        ]


{-| Standard Node Drawer. It can be configured using Rander.StandardDrawers.Attributes
-}
svgDrawNode : List (Attribute (NodeDrawerConfig n msg)) -> NodeDrawer n msg
svgDrawNode edits nodeAtrib =
    let
        node =
            nodeAtrib.node

        ( posX, posY ) =
            nodeAtrib.coord

        config =
            List.foldl (\f a -> f a) defNodeDrawerConfig edits

        lbl =
            config.label node

        nodeId =
            "node-" ++ String.fromInt node.id

        gAtrib =
            case config.onClick of
                Nothing ->
                    [ TA.id nodeId
                    , TA.class [ "node" ]
                    , TA.style <| config.style node
                    ]

                Just f ->
                    [ TA.id nodeId
                    , TA.class [ "node" ]
                    , TA.style <| config.style node
                    , TE.onClick (f node)
                    , TA.cursor TT.CursorPointer
                    ]
    in
    g
        gAtrib
        [ TS.title [] [ TC.text <| config.title node ]
        , nodeShapeDrawer config nodeAtrib
        , TS.text_
            [ TA.textAnchor TT.AnchorMiddle
            , TA.dominantBaseline TT.DominantBaselineCentral
            , TA.transform [ TT.Translate posX posY ]
            ]
            [ TC.text lbl ]
        , xLabelDrawer config.xLabel config.xLabelPos nodeAtrib
        ]



{- Gives coordinates of the edge adjusted with the srcMargin,TgtMargin and arrowhead lengths -}


getAdjustedSrcAndTarget : EdgeAttributes e -> Float -> Float -> List ( Float, Float )
getAdjustedSrcAndTarget edgeAtrib srcMargin tgtMargin =
    let
        pts =
            List.concat [ [ edgeAtrib.source ], edgeAtrib.controlPts, [ edgeAtrib.target ] ]

        ( sw, sh ) =
            edgeAtrib.sourceDimensions

        ( tw, th ) =
            edgeAtrib.targetDimensions

        src_to_next =
            List.take 2 pts

        target_from_previous =
            List.drop (List.length pts - 2) pts

        srcSeg =
            Curve.linear src_to_next |> SP.arcLengthParameterized tolerance

        tgtSeg =
            Curve.linear target_from_previous |> SP.arcLengthParameterized tolerance

        srcDim =
            (((sw ^ 2 + sh ^ 2) |> sqrt) / 2) + srcMargin

        tgtDim =
            (((tw ^ 2 + th ^ 2) |> sqrt) / 2) + tgtMargin

        final_src =
            SP.pointAlong srcSeg srcDim

        final_tgt =
            SP.pointAlong tgtSeg (SP.arcLength tgtSeg - tgtDim)
    in
    if edgeAtrib.edge.from == edgeAtrib.edge.to then
        pts

    else
        case ( final_src, final_tgt ) of
            ( Just s, Just t ) ->
                List.concat [ [ s ], edgeAtrib.controlPts, [ t ] ]

            ( Just s, Nothing ) ->
                List.concat [ [ s ], edgeAtrib.controlPts, [ edgeAtrib.target ] ]

            ( Nothing, Just t ) ->
                List.concat [ [ edgeAtrib.source ], edgeAtrib.controlPts, [ t ] ]

            ( Nothing, Nothing ) ->
                pts


arrowHeadDef : ArrowHeadShape -> Color.Color -> Svg msg
arrowHeadDef ahs stroke =
    case ahs of
        Triangle ->
            TS.defs [] [ triangleHeadElement stroke ]

        Vee ->
            TS.defs [] [ veeHeadElement stroke ]

        None ->
            TS.defs [] []


edgeLabelDrawer : String -> Bool -> String -> SP.ArcLengthParameterized c -> Svg msg
edgeLabelDrawer lbl orientLabelAlongEdge edgePathId curve =
    if orientLabelAlongEdge then
        TS.text_
            [ TA.textAnchor TT.AnchorMiddle
            , TA.dominantBaseline TT.DominantBaselineCentral
            ]
            [ TS.textPath [ TA.href ("#" ++ edgePathId), TA.startOffset "50%" ] [ TC.text lbl ] ]

    else
        let
            ( midX, midY ) =
                (case SP.pointAlong curve (SP.arcLength curve / 2) of
                    Just m ->
                        m

                    Nothing ->
                        ( -10, -10 )
                )
                    |> Tuple.mapBoth
                        (\a ->
                            if isNaN a then
                                -10

                            else
                                a
                        )
                        (\a ->
                            if isNaN a then
                                -10

                            else
                                a
                        )
        in
        TS.text_
            [ TA.textAnchor TT.AnchorMiddle
            , TA.dominantBaseline TT.DominantBaselineCentral
            , TA.transform [ TT.Translate midX midY ]
            ]
            [ TC.text lbl ]


nodeShapeDrawer : NodeDrawerConfig n msg -> NodeAttributes n -> Svg msg
nodeShapeDrawer config nodeAtrib =
    let
        ( posX, posY ) =
            nodeAtrib.coord

        width =
            nodeAtrib.width

        height =
            nodeAtrib.height

        d =
            max width height
    in
    case config.shape of
        Circle ->
            TS.circle
                [ TA.r <| TT.Px (d / 2)
                , TA.stroke <| TT.Paint <| config.strokeColor nodeAtrib.node
                , TA.strokeWidth <| TT.Px <| config.strokeWidth nodeAtrib.node
                , TA.strokeDasharray <| config.strokeDashArray nodeAtrib.node
                , TA.fill <| TT.Paint <| config.fill nodeAtrib.node
                , TA.cx <| TT.Px posX
                , TA.cy <| TT.Px posY
                ]
                []

        Ellipse ->
            TS.ellipse
                [ TA.rx <| TT.Px (width / 2)
                , TA.ry <| TT.Px (height / 2)
                , TA.stroke <| TT.Paint <| config.strokeColor nodeAtrib.node
                , TA.strokeWidth <| TT.Px <| config.strokeWidth nodeAtrib.node
                , TA.strokeDasharray <| config.strokeDashArray nodeAtrib.node
                , TA.fill <| TT.Paint <| config.fill nodeAtrib.node
                , TA.cx <| TT.Px posX
                , TA.cy <| TT.Px posY
                ]
                []

        Box ->
            TS.rect
                [ TA.width <| TT.Px width
                , TA.height <| TT.Px height
                , TA.stroke <| TT.Paint <| config.strokeColor nodeAtrib.node
                , TA.strokeWidth <| TT.Px <| config.strokeWidth nodeAtrib.node
                , TA.strokeDasharray <| config.strokeDashArray nodeAtrib.node
                , TA.fill <| TT.Paint <| config.fill nodeAtrib.node
                , TA.x <| TT.Px (posX - width / 2)
                , TA.y <| TT.Px (posY - height / 2)
                ]
                []

        RoundedBox r ->
            TS.rect
                [ TA.width <| TT.Px width
                , TA.height <| TT.Px height
                , TA.rx <| TT.Px r
                , TA.stroke <| TT.Paint <| config.strokeColor nodeAtrib.node
                , TA.strokeWidth <| TT.Px <| config.strokeWidth nodeAtrib.node
                , TA.strokeDasharray <| config.strokeDashArray nodeAtrib.node
                , TA.fill <| TT.Paint <| config.fill nodeAtrib.node
                , TA.x <| TT.Px (posX - width / 2)
                , TA.y <| TT.Px (posY - height / 2)
                ]
                []


xLabelDrawer : (Node n -> String) -> (Node n -> Float -> Float -> ( Float, Float )) -> NodeDrawer n msg
xLabelDrawer xlbl xLabelPos nodeAtrib =
    let
        ( posX, posY ) =
            nodeAtrib.coord

        ( xPosX, xPosY ) =
            xLabelPos nodeAtrib.node nodeAtrib.width nodeAtrib.height

        ( xlPosX, xlPosY ) =
            ( posX + xPosX, posY + xPosY )
    in
    TS.text_
        [ TA.textAnchor TT.AnchorMiddle
        , TA.dominantBaseline TT.DominantBaselineCentral
        , TA.transform [ TT.Translate xlPosX xlPosY ]
        ]
        [ TC.text (xlbl nodeAtrib.node)
        ]



{- Different svg elements for different heads on edges -}


triangleHeadElement : Color.Color -> Svg msg
triangleHeadElement stroke =
    TS.marker
        [ TA.id "triangle-head"
        , TA.viewBox 0 0 9 6
        , TA.markerWidth <| TT.Px 4.5
        , TA.markerHeight <| TT.Px 3
        , TA.refX "6"
        , TA.refY "3"
        , TA.orient "auto"
        , TA.markerUnits TT.MarkerCoordinateSystemStrokeWidth
        ]
        [ TS.polygon
            [ TA.points [ ( 0, 0 ), ( 0, 6 ), ( 9, 3 ) ]
            , TA.stroke <| TT.Paint stroke
            , TA.fill <| TT.Paint stroke
            ]
            []
        ]


veeHeadElement : Color.Color -> Svg msg
veeHeadElement stroke =
    TS.marker
        [ TA.id "vee-head"
        , TA.viewBox 0 0 9 6
        , TA.markerWidth <| TT.Px 4.5
        , TA.markerHeight <| TT.Px 3
        , TA.refX "6"
        , TA.refY "3"
        , TA.orient "auto"
        , TA.markerUnits TT.MarkerCoordinateSystemStrokeWidth
        ]
        [ TS.polygon
            [ TA.points [ ( 0, 0 ), ( 4.5, 3 ), ( 0, 6 ), ( 9, 3 ) ]
            , TA.stroke <| TT.Paint stroke
            , TA.fill <| TT.Paint stroke
            ]
            []
        ]
