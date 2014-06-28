module Constants where

import Vector as V

fps = 60
vectors = [ V.Vector 1 1 1
          , V.Vector 2 1 2
          , V.Vector 3 3 0
          , V.Vector 1 0 2 
          , V.Vector 0 1 1
          , V.Vector 0 1 2
          , V.Vector 1 1 2
          , V.Vector 1 3 2 ]

funs = [ "atom", "span", "project", "reject"]
defs = [ "An atom is the value behind a variable.\n\nEx. If b = Atom a,  a != b, there values coincide but they are not references to one another. If b = a, then any change to a will result in a change to b"
       , "A span is the set of all combinations of a vector or family of vectors"
       , "A projection is the component of a vector parallel to another vector"
       , "A rejection is the component of a vector perpendicular to another vector" 
       ]
vars = ["a", "b", "c", "d", "e", "f", "g"]
custom = ["+","-" {-,"?" -}]

