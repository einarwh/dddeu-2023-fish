module Picture exposing (..)

import Box exposing (..)
import Shape exposing (..)
import Style exposing (..)

type alias Rendering = List (Shape, Style)

type alias Picture = Box -> Rendering

blank : Picture 
blank _ = []

times : Int -> (a -> a) -> (a -> a)
times n fn = 
    if n < 1 then identity 
    else fn >> (times (n - 1) fn)

turn : Picture -> Picture
turn p = 
  turnBox >> p 

flip : Picture -> Picture
flip p = flipBox >> p 

toss : Picture -> Picture
toss p = tossBox >> p 

aboveRatio : Int -> Int -> Picture -> Picture -> Picture
aboveRatio m n p1 p2 = 
    \box -> 
        let 
            f = toFloat m / toFloat (m + n)
            (b1, b2) = splitVertically f box 
        in 
            (p1 b1) ++ (p2 b2)

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

-- column : create a column of pictures
column : List Picture -> Picture
column ps =
  case ps of 
    [] -> blank
    [p] -> p
    h::t -> aboveRatio 1 (List.length t) h (column t)

-- row : create a row of pictures
row : List Picture -> Picture
row ps = 
    case ps of 
        [] -> blank 
        [p] -> p 
        h :: t -> 
            besideRatio 1 (List.length t) h (row t)

-- quartet : 4 pictures arranged in 2 x 2 grid
quartet : Picture -> Picture -> Picture -> Picture -> Picture
quartet nw ne sw se = 
    above (beside nw ne) 
          (beside sw se)

iv p = quartet p p p p 

-- nonet : 9 pictures arranged in 3 x 3 grid
nonet : Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture
nonet nw nm ne mw mm me sw sm se = 
    column [ row [nw, nm, ne], row [mw, mm, me], row [sw, sm, se] ]

-- zoom : 9 pictures arranged in 3 x 3 grid with zoom effect
zoom : Int -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture
zoom n nw nm ne mw mm me sw sm se =
  if n < 1 then mm
  else
    let
      m = nonet nw nm ne mw mm me sw sm se
    in
      zoom (n - 1) nw nm ne mw m me sw sm se

zom : Int -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture
zom n nw nm ne mw mm me sw sm se =
  if n < 1 then mm
  else
    let
      m = nonet nw nm ne mw mm me sw sm se
    in
      zom (n - 1) m nm m mw m me m sm m

over : Picture -> Picture -> Picture
over p1 p2 = 
    \box -> (p1 box) ++ (p2 box)

ttile : Picture -> Picture
ttile fish = 
    let
        n = fish |> toss |> flip
        e = times 3 turn n
    in
        over fish (over n e)

utile : Picture -> Picture
utile fish =
    let
        n = fish |> toss |> flip 
        w = n |> turn 
        s = w |> turn 
        e = s |> turn 
    in
        n |> over w |> over s |> over e


utile4 : Picture -> Picture -> Picture -> Picture -> Picture
utile4 p1 p2 p3 p4 =
    let
        tf = toss >> flip
        n = p1 |> tf
        w = p2 |> tf |> turn 
        s = p3 |> tf |> times 2 turn 
        e = p4 |> tf |> times 3 turn 
    in
        n |> over w |> over s |> over e
    
-- cycle : a quartet with rotations
cycle : Picture -> Picture
cycle p = quartet p (p |> turn |> turn |> turn) (p |> turn) (p |> turn |> turn)    

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
            u = utile fish
        in
            quartet c s (turn s) u
        
squareLimit : Int -> Picture -> Picture
squareLimit n fish = 
    let
        nw = corner n fish 
        sw = nw |> turn 
        se = sw |> turn 
        ne = se |> turn 
        nm = side n fish 
        mw = nm |> turn 
        sm = mw |> turn 
        me = sm |> turn 
        mm = utile fish 
    in
        nonet nw nm ne mw mm me sw sm se 
    
ribbon : Int -> Picture -> Picture 
ribbon n fish = 
    if n < 1 then blank 
    else 
        let
            t = ttile fish |> turn |> turn 
            r = ribbon (n - 1) fish
            nw = t 
            ne = t |> turn
            sw = r 
            se = r 
        in
            quartet nw ne sw se 

smaller n fish = 
    if n < 1 then blank 
    else 
        let
            t = ttile fish 
            p = fish |> turn |> turn |> over fish |> turn
            r = smaller (n - 1) fish

        in
            quartet t r p t

andsmaller n fish = 
    let
        sw = smaller n fish 
        se = sw |> turn 
        ne = se |> turn 
        nw = ne |> turn 
    in
        quartet nw ne sw se  

smallerandsmaller n fish = 
    let
        t = ttile fish 
        sw = t 
        se = sw |> turn 
        ne = se |> turn 
        nw = ne |> turn 
        mm = andsmaller n fish 
        nm = quartet t (t |> turn) (t |> times 3 turn) (t |> times 2 turn)
        mw = turn nm 
        sm = turn mw 
        me = turn sm 
    in
        nonet nw nm ne mw mm me sw sm se 
