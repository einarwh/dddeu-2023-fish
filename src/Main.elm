module Main exposing (main)

import Box exposing (..)
import Picture exposing (..)
import Gather exposing (gatherBoxes)
import Recursion exposing (sideBoxes, cornerBoxes)
import Letter exposing (..)
import Figure exposing (..)
import Fishy exposing (fishShapes)
import Fitting exposing (createPicture)
import Html exposing (Html)
import Rendering exposing (BoxRendering(..))
import Decor exposing (render)

main : Html msg
main = 
  let 
    decor = 
      { coordinates = False
      , boxes = DrawOutline }
    box = { a = { dx = 100.0, dy = 100.0 }
          , b = { dx = 300.0, dy = 0.0 }
          , c = { dx = 0.0, dy = 300.0 } }
    fish = createPicture fishShapes
    n = 4
    boxes = cornerBoxes n box 
  in     
    box |> squareLimit n fish
        |> render [] decor