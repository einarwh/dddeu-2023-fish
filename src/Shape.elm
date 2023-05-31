module Shape exposing (..)

import Point exposing (..)

type alias LineShape = 
  { lineStart : Point
  , lineEnd : Point}

type alias PolygonShape = 
  { points : List Point }

type alias PolylineShape = 
  { pts : List Point }

type alias CurveShape = 
  { point1 : Point
  , point2 : Point
  , point3 : Point
  , point4 : Point }

type alias ArcShape = 
  { startPoint : Point
  , controlPoint : Point
  , endPoint : Point }

type alias BezierShape = 
  { controlPoint1 : Point
  , controlPoint2 : Point
  , endPoint : Point }

type Shape = 
    Line LineShape 
  | Polygon PolygonShape
  | Polyline PolylineShape
  | Curve CurveShape
  | Arc ArcShape
  | Path (Point, List BezierShape)
  