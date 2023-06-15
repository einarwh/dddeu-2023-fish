module Lizzy exposing (lizardShapes)

import Point exposing (Point)
import Shape exposing (Shape, CurveShape)

createPoint : Float -> Float -> Point 
createPoint x y = { x = x, y = y }

createCurve : Point -> Point -> Point -> Point -> CurveShape
createCurve v1 v2 v3 v4 = 
  { point1 = v1
  , point2 = v2
  , point3 = v3
  , point4 = v4 }

lizardCurves : List CurveShape
lizardCurves = [
    createCurve (createPoint 0.000 0.000) -- C1
                (createPoint 0.050 0.045) --
                (createPoint 0.100 0.090) --
                (createPoint 0.178 0.156) --  
  , createCurve (createPoint 0.178 0.156)
                (createPoint 0.190 0.165)                
                (createPoint 0.220 0.165  )
                (createPoint 0.237 0.148) 
  , createCurve (createPoint 0.237 0.148)
                (createPoint 0.260 0.130)
                (createPoint 0.310 0.100)
                (createPoint 0.367 0.067)
  , createCurve (createPoint 0.367 0.067)
                (createPoint 0.362 0.020)
                (createPoint 0.370 -0.030)
                (createPoint 0.400 -0.096)
  , createCurve (createPoint 0.400 -0.096)
                (createPoint 0.340 -0.087)
                (createPoint 0.280 -0.075)
                (createPoint 0.222 -0.044)
  , createCurve (createPoint 0.222 -0.044)
                (createPoint 0.205 -0.054)
                (createPoint 0.195 -0.064)
                (createPoint 0.185 -0.074)
  , createCurve (createPoint 0.185 -0.074)
                (createPoint 0.197 -0.103)
                (createPoint 0.195 -0.157)
                (createPoint 0.193 -0.237)
  , createCurve (createPoint 0.193 -0.237)
                (createPoint 0.320 -0.225)
                (createPoint 0.420 -0.200)
                (createPoint 0.474 -0.174)
  , createCurve (createPoint 0.474 -0.174)
                (createPoint 0.540 -0.140)
                (createPoint 0.540 -0.110)
                (createPoint 0.530 -0.089)
  , createCurve (createPoint 0.530 -0.089)
                (createPoint 0.500 -0.040)
                (createPoint 0.520 0.030)
                (createPoint 0.600 0.037)
  , createCurve (createPoint 0.600 0.037)
                (createPoint 0.700 0.045)
                (createPoint 0.800 0.030)
                (createPoint 1.000 0.000)
  , createCurve (createPoint 1.000 0.000)
                (createPoint 0.920 0.030)
                (createPoint 0.820 0.060)
                (createPoint 0.748 0.133)
  , createCurve (createPoint 0.748 0.133)
                (createPoint 0.720 0.160)
                (createPoint 0.710 0.210)
                (createPoint 0.760 0.244)
  , createCurve (createPoint 0.760 0.244)
                (createPoint 0.820 0.290)
                (createPoint 0.820 0.330)
                (createPoint 0.793 0.370)
  , createCurve (createPoint 0.793 0.370)
                (createPoint 0.773 0.400)
                (createPoint 0.723 0.460)
                (createPoint 0.674 0.511)
  , createCurve (createPoint 0.674 0.511)
                (createPoint 0.655 0.485)
                (createPoint 0.620 0.440)
                (createPoint 0.593 0.415)
  , createCurve (createPoint 0.593 0.415)
                (createPoint 0.620 0.395)
                (createPoint 0.650 0.375)
                (createPoint 0.681 0.363)
  , createCurve (createPoint 0.681 0.363)
                (createPoint 0.679 0.343)
                (createPoint 0.679 0.333)
                (createPoint 0.681 0.319)
  , createCurve (createPoint 0.681 0.319)
                (createPoint 0.640 0.310)
                (createPoint 0.620 0.290)
                (createPoint 0.585 0.259)
  , createCurve (createPoint 0.585 0.259)
                (createPoint 0.535 0.285)
                (createPoint 0.495 0.325)
                (createPoint 0.456 0.370)
  , createCurve (createPoint 0.456 0.370)
                (createPoint 0.415 0.430)
                (createPoint 0.480 0.480)
                (createPoint 0.500 0.500)
  , createCurve (createPoint 0.500 0.500)
                (createPoint 0.520 0.520)
                (createPoint 0.585 0.570)
                (createPoint 0.544 0.630)
  , createCurve (createPoint 0.544 0.630)
                (createPoint 0.505 0.675)
                (createPoint 0.465 0.715)
                (createPoint 0.415 0.741)
  , createCurve (createPoint 0.415 0.741)
                (createPoint 0.380 0.710)
                (createPoint 0.360 0.690)
                (createPoint 0.319 0.681)
  , createCurve (createPoint 0.319 0.681)
                (createPoint 0.321 0.667)
                (createPoint 0.321 0.657)
                (createPoint 0.319 0.637)
  , createCurve (createPoint 0.319 0.637)
                (createPoint 0.350 0.625)
                (createPoint 0.380 0.605)
                (createPoint 0.407 0.585)
  , createCurve (createPoint 0.407 0.585)
                (createPoint 0.380 0.560)
                (createPoint 0.345 0.515)
                (createPoint 0.326 0.489)
  , createCurve (createPoint 0.326 0.489)
                (createPoint 0.277 0.540)
                (createPoint 0.227 0.600)
                (createPoint 0.207 0.630)
  , createCurve (createPoint 0.207 0.630)
                (createPoint 0.180 0.670)
                (createPoint 0.180 0.710)
                (createPoint 0.240 0.756)
  , createCurve (createPoint 0.240 0.756)
                (createPoint 0.290 0.790)
                (createPoint 0.280 0.840)
                (createPoint 0.252 0.867)
  , createCurve (createPoint 0.252 0.867)
                (createPoint 0.180 0.940)
                (createPoint 0.080 0.970)
                (createPoint 0.000 1.000)

   ]

--[[(0.0, 1.0), (0.080, 0.97), (0.180, 0.94), (0.252, 0.867)], 
--[(0.252, 0.867), (0.28, 0.84), (0.290, 0.79), (0.24, 0.756)], 
--[(0.24, 0.756), (0.180, 0.71), (0.180, 0.670), (0.207, 0.63)], 
--[(0.207, 0.63), (0.227, 0.6), (0.277, 0.54), (0.326, 0.489)], 
--[(0.326, 0.489), (0.345, 0.515), (0.38, 0.56), (0.407, 0.585)], 
--[(0.407, 0.585), (0.38, 0.605), (0.35, 0.625), (0.319, 0.637)], 
--[(0.319, 0.637), (0.321, 0.657), (0.321, 0.667), (0.319, 0.681)], 
--[(0.319, 0.681), (0.36, 0.69), (0.38, 0.71), (0.415, 0.741)], 
--[(0.415, 0.741), (0.465, 0.715), (0.505, 0.675), (0.544, 0.63)], 
--[(0.544, 0.63), (0.585, 0.570), (0.52, 0.52), (0.5, 0.5)]]


lizardShapes : List Shape
lizardShapes = lizardCurves |> List.map Shape.Curve