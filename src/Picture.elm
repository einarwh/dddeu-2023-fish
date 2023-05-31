module Picture exposing (..)

import Box exposing (..)
import Shape exposing (..)
import Style exposing (..)

type alias Rendering = List (Shape, Style)

type alias Picture = Box -> Rendering

blank : Picture 
blank _ = []

-- Box -> Rendering 
-- Box -> Box 
turn : Picture -> Picture
turn p = turnBox >> p 

times : Int -> (a -> a) -> (a -> a)
times n fn = 
    if n < 1 then identity
    else fn >> times (n - 1) fn 

flip : Picture -> Picture
flip p = flipBox >> p 

toss : Picture -> Picture
toss p = tossBox >> p 

aboveRatio : Int -> Int -> Picture -> Picture -> Picture
aboveRatio m n p1 p2 = 
    \box -> 
        let 
            f = toFloat m / toFloat (m + n) 
            (top, bot) = splitVertically f box
        in 
            (p1 top) ++ (p2 bot) 

above : Picture -> Picture -> Picture
above = aboveRatio 1 1

besideRatio : Int -> Int -> Picture -> Picture -> Picture
besideRatio m n p1 p2 =
  \box ->
    let
      f = toFloat m / toFloat (m + n)
      (b1, b2) = splitHorizontally f box
    in
      (p1 b1) ++ (p2 b2)

beside : Picture -> Picture -> Picture
beside = besideRatio 1 1

-- quartet : 4 pictures arranged in 2 x 2 grid
quartet : Picture -> Picture -> Picture -> Picture -> Picture
quartet nw ne sw se = 
    above (beside nw ne)
          (beside sw se)

-- nonet : 9 pictures arranged in 3 x 3 grid
nonet : Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture
nonet nw nm ne mw mm me sw sm se = 
    let 
        row w m e = besideRatio 1 2 w (beside m e)
        col n m s = aboveRatio 1 2 n (above m s)
    in 
        col (row nw nm ne)
            (row mw mm me)
            (row sw sm se)

over : Picture -> Picture -> Picture
over p1 p2 = 
    \box -> (p1 box) ++ (p2 box)

ttile : Picture -> Picture
ttile fish = 
    let 
        fishN = fish |> toss |> flip
        fishE = fishN |> turn |> turn |> turn
    in 
        fishE |> over fishN |> over fish   

utile : Picture -> Picture
utile fish = 
    let 
        fishN = fish |> toss |> flip 
        fishW = turn fishN 
        fishS = turn fishW 
        fishE = turn fishS 
    in 
        fishE |> over fishS |> over fishW |> over fishN 

side : Int -> Picture -> Picture
side n fish = 
    if n < 1 then blank
    else 
        let 
            s = side (n - 1) fish
            t = ttile fish
        in 
            quartet s s (turn t) t 

corner : Int -> Picture -> Picture
corner n fish =
    if n < 1 then blank
    else 
        let 
            c = corner (n - 1) fish 
            s = side (n - 1) fish
        in 
            quartet c s (turn s) (utile fish)

squareLimit : Int -> Picture -> Picture
squareLimit n fish =
    let 
        nw = corner n fish 
        sw = turn nw 
        se = turn sw 
        ne = turn se
        nm = side n fish 
        mw = turn nm
        sm = turn mw
        me = turn sm
        mm = utile fish
    in 
        nonet nw nm ne mw mm me sw sm se
        