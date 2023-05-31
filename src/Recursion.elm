module Recursion exposing (sideBoxes, cornerBoxes)

import Box exposing (..)

sideBoxes : Int -> Box -> List Box
sideBoxes n box =
  if n < 1 then [ box ]
  else
    let
      (top, bot) = splitVertically 0.5 box
      (nw, ne) = splitHorizontally 0.5 top
      (sw, se) = splitHorizontally 0.5 bot
    in
      sideBoxes (n - 1) nw ++ sideBoxes (n - 1) ne ++ [sw, se]

westSideBoxes : Int -> Box -> List Box
westSideBoxes n box =
  if n < 1 then [ box ]
  else
    let
      (top, bot) = splitVertically 0.5 box
      (nw, ne) = splitHorizontally 0.5 top
      (sw, se) = splitHorizontally 0.5 bot
    in
      westSideBoxes (n - 1) nw ++ westSideBoxes (n - 1) sw ++ [ne, se]

cornerBoxes : Int -> Box -> List Box
cornerBoxes n box =
  if n < 1 then [ box ]
  else
    let
      (top, bot) = splitVertically 0.5 box
      (nw, ne) = splitHorizontally 0.5 top
      (sw, se) = splitHorizontally 0.5 bot
    in
      cornerBoxes (n - 1) nw ++ sideBoxes (n - 1) ne ++ westSideBoxes (n - 1) sw ++ [se]
