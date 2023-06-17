module Main exposing (main)

import Box exposing (..)
import Picture exposing (..)
import Gather exposing (gatherBoxes)
import Recursion exposing (sideBoxes, cornerBoxes)
import Letter exposing (..)
import Figure exposing (..)
import Fishy exposing (fishShapes)
import Lizzy exposing (lizardShapes)
import Fitting exposing (createPicture)
import Html exposing (Html)
import Rendering exposing (BoxRendering(..))
import Decor exposing (render)
import Lizzy exposing (lizardShapes)

main : Html msg
main = 
  let 
    decor = 
      { coordinates = False
      , boxes = DrawNothing }
    box = { a = { dx = 50.0, dy = 100.0 }
          , b = { dx = 300.0, dy = 0.0 }
          , c = { dx = 0.0, dy = 300.0 } }
    liz = createPicture lizardShapes
    fish = createPicture fishShapes
    n = 4
    p = over liz (times 2 turn liz)
  in     
    box |> smallerandsmaller 6 fish
        |> render [ box ] decor