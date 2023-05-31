module Main exposing (main)

import Box exposing (..)
import Picture exposing (..)
import Gather exposing (gatherBoxes)
import Recursion exposing (sideBoxes, cornerBoxes)
import Letter exposing (..)
import Figure exposing (..)
import Fishy exposing (fishShapes)
import Triangular exposing (..)
import Fitting exposing (createPicture)
import Html exposing (Html)
import Rendering exposing (BoxRendering(..))
import Decor exposing (render)

main : Html msg
main = 
  let 
    decor = 
      { coordinates = True
      , boxes = DrawOutline }
    box = { a = { dx = 100.0, dy = 100.0 }
          , b = { dx = 200.0, dy = 0.0 }
          , c = { dx = 0.0, dy = 200.0 } }
    f = createPicture fLetter
    n = 4
    p = times n toss f
    boxes = gatherBoxes n tossBox box
  in     
    box |> blank
        |> render boxes decor