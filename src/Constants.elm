module Constants where

{-
    TODO:
    add +/- to vars?
-}

import Vector as V

fps = 60
values = [ V.Vector 1 1 1
         , V.Vector 2 1 2
         , V.Vector 3 3 0
         , V.Vector 1 0 2 
         , V.Vector 0 1 1
         , V.Vector 0 1 2
         , V.Vector 1 1 2]

expressions = [ [ 0, 0 ] 
              , [ 0, 1 ] 
              , [ 0, 2 ]
              , [ 0, 3 ]
              , [ 0, 4 ]
              , [ 0, 5 ]
              , [ 0, 6 ]]

funs = [ "add", "subtract", "project", "reject", "unit", "scale", "rotate", "trace"]
meta = [ "save", "set", "reset", "yes", "no"]
defs = [ "An atom is the value behind a variable."
       , "A span is the set of all combinations of a vector or family of vectors"
       , "A projection is the component of a vector parallel to another vector"
       , "A rejection is the component of a vector perpendicular to another vector" 
       ]
vars = ["a", "b", "c", "d", "e", "f", "g" {-, "+", "-" -}]
lambda = "&#955"

-- ids
restState = 0
hoverState = 1
clickState = 2
hideState = 3
