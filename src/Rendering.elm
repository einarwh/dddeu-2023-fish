module Rendering exposing (toSvg, toSvgWithBoxes, Decoration, BoxRendering(..))

import Point exposing (..)
import Vector exposing (..)
import Shape exposing (..)
import Style exposing (..)
import Box exposing (..)
import Picture exposing (..)
import Mirror exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

type BoxRendering = DrawOutline | DrawVectors | DrawNothing

type alias Decoration = 
  { coordinates : Bool
  , boxes : BoxRendering }

--useArrows = True

dottedLineColor = "grey"
solidLineColor = "red"

toString : Float -> String 
toString = String.fromFloat

getStrokeWidthFromStyle : Maybe StrokeStyle -> Float
getStrokeWidthFromStyle style = 
  case style of 
    Just { strokeWidth } -> sqrt strokeWidth
    Nothing -> 2.0

toPolygonElement : Style -> List Point -> Svg msg
toPolygonElement style pts = 
  let 
    s = 
      let 
        str {x, y} = (String.fromFloat x) ++ "," ++ (String.fromFloat y)
      in
        pts |> List.map str |> String.join " "
    sw = getStrokeWidthFromStyle style.stroke  
  in
    Svg.polygon 
      [ stroke "Black"
      , strokeWidth <| String.fromFloat sw
      , fill "None"
      , points s ] []

toPolylineElement : Style -> List Point -> Svg msg
toPolylineElement style pts = 
  let 
    s = 
      let 
        str {x, y} = (String.fromFloat x) ++ "," ++ (String.fromFloat y)
      in
        pts |> List.map str |> String.join " "
    sw = getStrokeWidthFromStyle style.stroke  
  in
    Svg.polyline 
      [ stroke "Black"
      , strokeWidth <| String.fromFloat sw
      , fill "None"
      , points s ] []

toCurveElement : Style -> Point -> Point -> Point -> Point -> Svg msg
toCurveElement style pt1 pt2 pt3 pt4 = 
  let 
    toStr {x, y} = (toString x) ++ " " ++ (toString y)
    pt1s = toStr pt1
    pt2s = toStr pt2 
    pt3s = toStr pt3 
    pt4s = toStr pt4 
    dval = "M" ++ pt1s ++ " C " ++ pt2s ++ ", " ++ pt3s ++ ", " ++ pt4s
    sw = getStrokeWidthFromStyle style.stroke  
  in 
    Svg.path 
      [ stroke "Black"
      , strokeWidth <| toString sw
      , fill "None"
      , d dval ] []

toArcElement : Style -> Point -> Point -> Point -> Svg msg
toArcElement style pt1 pt2 pt3 = 
  let 
    toStr {x, y} = (toString x) ++ " " ++ (toString y)
    pt1s = toStr pt1
    pt2s = toStr pt2 
    pt3s = toStr pt3 
    dval = "M" ++ pt1s ++ " Q " ++ pt2s ++ " " ++ pt3s
    sw = getStrokeWidthFromStyle style.stroke  
  in 
    Svg.path 
      [ stroke "Black"
      , strokeWidth <| toString sw
      , fill "None"
      , d dval ] []

toSvgElement : Style -> Shape -> Svg msg
toSvgElement style shape = 
  case shape of  
    Polygon { points } -> toPolygonElement style points    
    Polyline { pts } -> toPolylineElement style pts
    Curve { point1, point2, point3, point4 } ->
      toCurveElement style point1 point2 point3 point4 
    Arc { startPoint, controlPoint, endPoint } -> 
      toArcElement style startPoint controlPoint endPoint
    x -> text "nothing"

toDottedBoxPolylineElement : List Point -> Svg msg
toDottedBoxPolylineElement pts = 
  let 
    s = 
      let 
        str {x, y} = (toString x) ++ "," ++ (toString y)
      in
        pts |> List.map str |> String.join " "
    sw = 1
  in
    Svg.polyline 
      [ stroke dottedLineColor
      , strokeWidth <| toString sw
      , strokeDasharray "2,2"
      , fill "None"
      , points s ] []

toSolidBoxPolylineElement : List Point -> Svg msg
toSolidBoxPolylineElement pts = 
  let 
    s = 
      let 
        str {x, y} = (toString x) ++ "," ++ (toString y)
      in
        pts |> List.map str |> String.join " "
    sw = 1
  in
    Svg.polyline 
      [ stroke solidLineColor
      , strokeWidth <| toString sw
      , fill "None"
      , points s ] []

toBoxPolylineElement : BoxRendering -> List Point -> List (Svg msg) 
toBoxPolylineElement rendering points = 
  case rendering of 
    DrawVectors -> [ toDottedBoxPolylineElement points ]
    DrawOutline -> [ toSolidBoxPolylineElement points ]
    DrawNothing -> []

toBoxLine : (Point -> Point) -> Point -> Point -> (String, String) -> Svg msg
toBoxLine m p1 p2 (name, color) = 
  let 
    w1 = m p1 
    w2 = m p2 
  in
    Svg.line 
      [ x1 <| toString w1.x
      , y1 <| toString w1.y 
      , x2 <| toString w2.x
      , y2 <| toString w2.y
      , stroke color
      , strokeWidth "1.5"
      , markerEnd <| "url(#" ++ name ++ ")" ] []

acolor : String 
acolor = "#b22327"

bcolor : String 
bcolor = "#2381bf"

ccolor : String 
ccolor = "#27b15b"

vectorToPoint : Vector -> Point 
vectorToPoint { dx, dy } = { x = dx, y = dy } 

toBoxArrows : (Point -> Point) -> Box -> List (Svg msg)
toBoxArrows m { a, b, c } =
  let
    pt0 = { x = 0, y = 0 }
    pta = vectorToPoint a
    ptb = vectorToPoint (add a b)
    ptc = vectorToPoint (add a c) 
  in
    [ toBoxLine m pt0 pta ("a-arrow", acolor)
    , toBoxLine m pta ptb ("b-arrow", bcolor)
    , toBoxLine m pta ptc ("c-arrow", ccolor) ]

toBoxShape : (Point -> Point) -> Box -> List Point
toBoxShape m { a, b, c } = 
  let
    b2 = add a b
    c2 = add a c 
    d = add a (add b c)
    pt0 = vectorToPoint a
    pt1 = vectorToPoint b2
    pt2 = vectorToPoint d
    pt3 = vectorToPoint c2
    pts = [m pt0, m pt1, m pt2, m pt3, m pt0]
  in
    pts

toBoxArrowLines : (Vector -> Vector) -> Box -> List Vector
toBoxArrowLines m { a, b, c } = 
  let
    b2 = add a b
    c2 = add a c 
    d = add a (add b c)
    pts = [m a, m b2, m d, m c2, m a]
  in
    pts

createAxisList : Int -> Int -> List Int 
createAxisList interval n = 
  if n < 0 then []
  else 
    let next = n - interval
    in 
      n :: createAxisList interval next

createXAxisElement : Int -> Int -> Svg msg
createXAxisElement y x = 
  Svg.line 
    [ x1 <| String.fromInt x
    , y1 <| String.fromInt y
    , x2 <| String.fromInt x
    , y2 <| String.fromInt (y - 3)
    , stroke "black"
    , strokeWidth "1.0" ] []

createXAxis : Int -> Int -> List (Svg msg) 
createXAxis xmax ypos = 
  let 
    axisList = createAxisList 20 xmax
  in 
    axisList |> List.map (\x -> xmax - x) |> List.map (createXAxisElement ypos)

createYAxisElement : Int -> Int -> Svg msg
createYAxisElement x y = 
  Svg.line 
    [ x1 <| String.fromInt x
    , y1 <| String.fromInt y
    , x2 <| String.fromInt (x + 3)
    , y2 <| String.fromInt y
    , stroke "black"
    , strokeWidth "1.0" ] []

createYAxis : Int -> Int -> List (Svg msg) 
createYAxis xpos ymax = 
  let 
    axisList = createAxisList 20 ymax
  in 
    axisList |> List.map (createYAxisElement xpos)

createMarker : (String, String) -> Svg msg
createMarker (markerId, color) = 
  Svg.marker 
    [ id markerId
    , markerWidth "10"
    , markerHeight "10"
    , refX "9"
    , refY "3"
    , orient "auto"
    , markerUnits "strokeWidth" ] 
    [ Svg.path 
      [ d "M0,0 L0,6 L9,3 z"
      , fill color ] [] ]

createAxes : Int -> Int -> List (Svg msg)
createAxes w h = 
  let 
    xx = String.fromInt 0
    yy = String.fromInt h
    axisColor = "black"
    xAxis =     
      Svg.line 
        [ x1 <| String.fromInt 0
        , y1 yy
        , x2 <| String.fromInt w
        , y2 yy
        , stroke axisColor
        , strokeWidth "1.5" ] []
    yAxis =  
      Svg.line 
        [ x1 xx
        , y1 <| String.fromInt 0
        , x2 xx
        , y2 <| String.fromInt w
        , stroke axisColor
        , strokeWidth "1.5" ] []
  in 
    [ xAxis, yAxis ] ++ createXAxis w h ++ createYAxis 0 h

toSvgWithBoxes : (Int, Int) -> List Box -> Decoration -> Rendering -> Svg msg 
toSvgWithBoxes bounds boxes decoration rendering = 
  let
    (w, h) = bounds
    viewBoxValue = ["0", "0", String.fromInt w, String.fromInt h] |> String.join " "
    mirror = mirrorPoint <| toFloat h
    boxShapes = boxes |> List.map (toBoxShape mirror) |> List.concatMap (toBoxPolylineElement decoration.boxes)
    boxArrows = boxes |> List.concatMap (toBoxArrows mirror)
    boxLines = 
      case decoration.boxes of 
        DrawVectors -> boxShapes ++ boxArrows 
        _ -> boxShapes
    toElement (shape, style) = toSvgElement style (mirrorShape mirror shape)
    things = rendering |> List.map toElement
    axes = 
      if decoration.coordinates then (createAxes w h) else []
    markers =
      [ ("a-arrow", acolor)
      , ("b-arrow", bcolor)
      , ("c-arrow", ccolor) ]
    defs = markers |> List.map createMarker |> Svg.defs []
    svgElements = 
      case boxes of 
      [] -> things ++ axes
      _ -> ([defs] ++ things ++ boxLines ++ axes)
  in
    svg
      [ version "1.1"
      , x "0"
      , y "0"
      , width (String.fromInt w)
      , height (String.fromInt h) ]
      svgElements

toSvg : (Int, Int) -> Decoration -> Rendering -> Svg msg 
toSvg bounds decoration rendering = 
  toSvgWithBoxes bounds [] decoration rendering 
