module Triangular exposing (..)

import Point exposing (Point)
import Shape exposing (Shape)
import Mirror exposing (mirrorShape)

rotate : Float -> Point -> Point 
rotate d { x, y } = 
  let 
    r = degrees d
    c = cos r
    s = sin r
  in 
    { x = x * c - y * s
    , y = y * c + x * s }

shrink : Float -> Point -> Point 
shrink factor { x, y } = 
  { x = factor * x, y = factor * y }

flip : Point -> Point
flip { x, y } = { x = 1 - x, y = y }

vflip : Point -> Point
vflip { x, y } = { x = x, y = 1 - y }

negvflip : Point -> Point
negvflip { x, y } = { x = x, y = -y }

move : Point -> Point -> Point 
move off { x, y } = 
      { x = x + off.x, y = y + off.y }

transform : Float -> Point -> Point -> Point
transform d offset = 
  flip >> shrink (1 / (sqrt 2)) >> rotate d >> move offset

derive1 : Shape -> Shape 
derive1 = mirrorShape (rotate 90)

derive2 : Shape -> Shape 
derive2 = mirrorShape (flip >> shrink (1 / (sqrt 2)) >> rotate 315 >> move { x = 0, y = 1 })

derive3 : Shape -> Shape 
derive3 = mirrorShape (flip >> shrink (1 / (sqrt 2)) >> rotate 135 >> move { x = 1, y = 0 })

createPoint : Float -> Float -> Point 
createPoint x y = { x = x, y = y }

createCurve : Point -> Point -> Point -> Point -> Shape
createCurve v1 v2 v3 v4 = 
  let c = { point1 = v1
          , point2 = v2
          , point3 = v3
          , point4 = v4 }
  in 
    Shape.Curve c

createArc : Point -> Point -> Point -> Shape
createArc v1 v2 v3 = 
  let a = { startPoint = v1
          , controlPoint = v2
          , endPoint = v3 }
  in 
    Shape.Arc a

createCircleCurve : Float -> Point -> Shape
createCircleCurve size centerPoint = 
  let
    offset = (4/3)*tan(pi/8) -- == 0.552284749831
    pt1 = createPoint 0.000 0.000
    pt2 = createPoint 0.000 (offset * size)
    pt3 = createPoint ((1 - offset) * size) size
    pt4 = createPoint size size
    c = { point1 = pt1
        , point2 = pt2
        , point3 = pt3
        , point4 = pt4 }
  in 
    Shape.Curve c |> mirrorShape (move centerPoint)

createLine : Point -> Point -> Shape
createLine v1 v2 = 
  Shape.Polyline { pts = [v1, v2] }

curveShapes : List Shape
curveShapes = 
  let 
    crv = createCurve (createPoint 0.000 0.000) -- C1
                      (createPoint 0.250 0.250) --
                      (createPoint 0.500 0.250) --
                      (createPoint 1.000 0.250) -- 
    make c = 
      let 
        c1 = derive1 c
        c2 = derive2 c
        c3 = derive3 c
      in 
        [c, c1, c2]
  in 
    make crv

triangulate : List Shape -> List Shape 
triangulate shapes = 
  let 
    shapes1 = List.map derive1 shapes
    shapes2 = List.map derive2 shapes 
    shapes3 = List.map derive3 shapes 
  in 
    shapes ++ shapes1 ++ shapes2 ++ shapes3

primitiveFishShapes : List Shape
primitiveFishShapes = 
  let 
    line1 = createLine (createPoint 0.000 0.000)
                       (createPoint 0.250 0.200)
    line2 = createLine (createPoint 0.250 0.200)
                       (createPoint 0.500 0.000)
    line3 = createLine (createPoint 0.500 0.000)
                       (createPoint 0.750 0.075)
    line4 = createLine (createPoint 0.750 0.075)
                       (createPoint 1.000 0.000)
    lines = [ line1, line2, line3, line4 ]
  in 
    triangulate lines

createTop : Float -> Float -> Float -> Shape 
createTop height start stop = 
  let 
    start0 = start 
    stop0 = stop
    middle = start + (stop - start) / 2
    pt0 = { x = start, y = 0 }
    pt1 = { x = middle, y = height }
    pt2 = { x = stop, y = 0 }
  in 
    Shape.Polyline { pts = [ pt0, pt1, pt2 ] }
    
floweryShapes : Int -> List Shape
floweryShapes n = 
  let 
    t = tan (degrees 22.5) / 2
    dt = t / toFloat n
    numbers = List.range -n n
    create i = createTop (toFloat i * dt) 0.000 1.000
    lines = List.map create numbers 
  in 
    triangulate lines

boomerangShapes : Int -> List Shape
boomerangShapes n = 
  let 
    t = tan (degrees 22.5) / 2
    dt = t / toFloat n
    numbers = List.range 1 n |> List.filter (\x -> abs x > n - 2)
    create i = createTop (toFloat i * dt) 0.000 1.000
    lines = List.map create numbers 
  in 
    triangulate lines

wrongFlowerShapes : List Shape
wrongFlowerShapes = 
  let 
    t = tan (degrees 22.5)
    dt = t / 3
    h0 = 0
    h1 = dt 
    h2 = 2 * dt
    h3 = t
    line0  = createTop h0 0.000 1.000
    line1a = createTop h1 0.000 1.000
    line2a = createTop h2 0.000 1.000
    line3a = createTop h3 0.000 1.000
    line1b = createTop -h1 0.000 1.000
    line2b = createTop -h2 0.000 1.000
    line3b = createTop -h3 0.000 1.000
    lines = [ line0, line1a, line2a, line3a, line1b, line2b, line3b ]
  in 
    triangulate lines

createCircley : Float -> List Shape 
createCircley maxSize = 
  let 
    offset = (4/3)*tan(pi/8) -- == 0.552284749831
    crv1 = createCircleCurve maxSize { x = (0.500 - maxSize), y = 0.0}
    crv2 = mirrorShape flip crv1
    crv3 = mirrorShape negvflip crv1
    crv4 = mirrorShape negvflip crv2
    arcs = [ crv1, crv2, crv3, crv4 ]
  in 
    arcs

createCircleys : Int -> Float -> List Shape 
createCircleys n maxSize = 
  let 
    numbers = List.range 1 n |> List.map toFloat
    delta = maxSize / toFloat n
    circles = List.concatMap (\i -> createCircley (i * delta))numbers
  in 
    circles

circleyShapes : Int -> Float -> List Shape 
circleyShapes n maxSize = 
  triangulate (createCircleys n maxSize)

circleyShapes2 : Int -> Float -> List Shape 
circleyShapes2 n maxSize = 
  triangulate (createCircleys n maxSize)

circleShapes : List Shape 
circleShapes = 
  let 
    offset = (4/3)*tan(pi/8) -- == 0.552284749831
    crv1 = createCircleCurve 0.200 { x = 0.300, y = 0.0}
    crv2 = mirrorShape flip crv1
    crv3 = mirrorShape negvflip crv1
    crv4 = mirrorShape negvflip crv2
    arcs = [ crv1, crv2, crv3, crv4 ]
  in 
    triangulate arcs

circleShapes2 : List Shape 
circleShapes2 = 
  let 
    offset = (4/3)*tan(pi/8) -- == 0.552284749831
    crv1 = createCircleCurve 0.500 { x = 0.000, y = 0.0}
    crv2 = mirrorShape flip crv1
    crv3 = mirrorShape negvflip crv1
    crv4 = mirrorShape negvflip crv2
    arcs = [ crv1, crv2, crv3, crv4 ]
  in 
    triangulate arcs
    
eyeShapes : List Shape 
eyeShapes = 
  let 
    offset = (4/3)*tan(pi/8) -- == 0.552284749831
    cs = createCircleys 3 0.105
    arc1 = createArc (createPoint 0.0 0.0)
                     (createPoint 0.5 0.22)
                     (createPoint 1.0 0.0)
    arc2 = mirrorShape negvflip arc1
    arcs = arc1 :: arc2 :: cs
  in 
    triangulate arcs

birdShapes : List Shape 
birdShapes = 
  let 
    crv1 = createCurve (createPoint 0.000 0.000)
                       (createPoint 0.159 0.051)
                       (createPoint 0.220 0.112)
                       (createPoint 0.266 0.243)
    crv2 = createCurve (createPoint 0.266 0.243)
                       (createPoint 0.364 0.224)
                       (createPoint 0.430 0.187)
                       (createPoint 0.491 0.107)
    crv3 = createCurve (createPoint 0.491 0.107)
                       (createPoint 0.561 0.019)
                       (createPoint 0.650 0.000)
                       (createPoint 0.750 0.022)
    crv4 = createCurve (createPoint 0.750 0.022)
                       (createPoint 0.832 0.038)
                       (createPoint 0.899 0.035)
                       (createPoint 1.000 0.000)
  in 
    triangulate [crv1, crv2, crv3, crv4] 