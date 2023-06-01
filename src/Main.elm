module Main exposing (main)

import Box exposing (..)
import Picture exposing (..)
import Gather exposing (..)
-- import Recursion exposing (sideBoxes, cornerBoxes)
import Letter exposing (..)
import Figure exposing (..)
import Fishy exposing (..)
import Fitting exposing (createPicture)
import Html exposing (Html)
import Rendering exposing (BoxRendering(..))
import Decor exposing (render)
import Box exposing (..)

main : Html msg
main = 
  let 
    decor = 
      { coordinates = True
      , boxes = DrawVectors }
    box = { a = { dx = 100.0, dy = 100.0 }
          , b = { dx = 300.0, dy = 0.0 }
          , c = { dx = 0.0, dy = 300.0 } }
    f = createPicture fLetter
  in     
    box |> f
        |> render [ box ] decor