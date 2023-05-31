module Vector exposing (..)

type alias Vector = 
    { dx : Float
    , dy : Float }

add : Vector -> Vector -> Vector 
add v1 v2 = 
    { dx = v1.dx + v2.dx
    , dy = v1.dy + v2.dy }

neg : Vector -> Vector 
neg { dx, dy } = 
    { dx = -dx
    , dy = -dy }

sub : Vector -> Vector -> Vector 
sub v1 v2 = add v1 (neg v2)

scale : Float -> Vector -> Vector
scale f { dx, dy } = 
    { dx = f * dx
    , dy = f * dy }

length : Vector -> Float 
length { dx, dy } = sqrt (dx * dx + dy * dy)
