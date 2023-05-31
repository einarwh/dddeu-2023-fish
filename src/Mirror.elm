module Mirror exposing (..) 

import Point exposing (Point)
import Shape exposing (..)

mirrorPoint : Float -> Point -> Point 
mirrorPoint height { x, y } = 
  { x = x, y = height - y }

mirrorShape : (Point -> Point) -> Shape -> Shape
mirrorShape mirror shape = 
  case shape of  
    Line { lineStart, lineEnd } -> 
      Line { lineStart = mirror lineStart, lineEnd = mirror lineEnd }
    Polygon { points } -> 
      Polygon { points = points |> List.map mirror }
    Polyline { pts } -> 
      Polyline { pts = pts |> List.map mirror }
    Curve { point1, point2, point3, point4  } ->
      Curve { point1 = mirror point1
            , point2 = mirror point2 
            , point3 = mirror point3 
            , point4 = mirror point4 }
    Arc { startPoint, controlPoint, endPoint } -> 
      Arc { startPoint = mirror startPoint
          , controlPoint = mirror controlPoint 
          , endPoint = mirror endPoint }
    x -> x