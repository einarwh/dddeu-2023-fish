module Gather exposing (gatherBoxes)

import Box exposing (..)

-- gatherBoxes : gather intermediate transformed boxes
gatherBoxes : Int -> (Box -> Box) -> Box -> List Box
gatherBoxes n transform box =
  if n < 1 then [ box ]
  else
    box :: gatherBoxes (n - 1) transform (transform box)

