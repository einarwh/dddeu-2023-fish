module Fitting exposing (createPicture)

import Vector exposing (Vector, add, scale, length) 
import Point exposing (..)
import Box exposing (Box)
import Shape exposing (..)
import Style exposing (..)
import Picture exposing (Picture)

mapper : Box -> Point -> Point 
mapper { a, b, c } { x, y } =
  let { dx, dy } = add a (add (scale x b) (scale y c))
  in { x = dx, y = dy}

getStrokeWidth : Box -> Float
getStrokeWidth { b, c } =
  let 
    s = max (length b) (length c) 
  in
    s / 80.0

getStyle : Box -> Style
getStyle box = 
  let sw = getStrokeWidth box in
  { stroke = Just { strokeWidth = sw
                  , strokeColor = Black } 
  , fill = Nothing }

mapShape : (Point -> Point) -> Shape -> Shape 
mapShape m shape = 
  case shape of  
    Polygon { points } -> Polygon { points = List.map m points }
   
    Polyline { pts } -> Polyline { pts = List.map m pts }

    Curve { point1, point2, point3, point4 } ->
      Curve { point1 = m point1 
            , point2 = m point2 
            , point3 = m point3 
            , point4 = m point4 } 

    Arc { startPoint, controlPoint, endPoint } ->
      Arc { startPoint = m startPoint
          , controlPoint = m controlPoint 
          , endPoint = m endPoint } 

    x -> x

createPicture : List Shape -> Picture
createPicture shapes box = 
  let 
    m = mapper box
    style = getStyle box
  in 
    shapes |> List.map (mapShape m) |> List.map (\s -> (s, style))
    