module Constants where

import Array as A

import Expr as E
import Vector as V


{-
    TODO:
    add +/- to vars?
-}

model = { 
        -- shared
        expr = E.Empty--E.Node 1 ([E.Leaf 0,E.Leaf 1])--E.Empty 
        , value = V.Abyss
        , exprs = expressions -- [[functionID, varID,..]], list of var expr
        , values = values -- list of vectors behind all expressions
        -- ui part 
        , funs = (A.repeat (length funs) 0)
        , vars = (A.repeat (length vars) 0) --  transparent state
        , meta = (A.fromList [2,2,2,2,2,3,3]) --A.repeat (length meta) 2 -- transparent state
        , index = 0 -- index of expr
        , history = [] -- buffer of buttons which can be undone
        , base = [] -- buffer of commited changes
        -- graph part
        , basis  = [(0,40),(0,50),(50,0)]
        , target = "" 
        , units = 1
        }

historyLimit = 20
fps = 60
rotate = False
values = [ V.Vector 1 1 1
         , V.Vector 2 1 2
         , V.Vector 3 3 0
         , V.Vector 1 0 2 ]
--       , V.Vector 0 1 1
--       , V.Vector 0 1 2
--       , V.Vector 1 1 2]
expressions = [ [ 0, 0 ] 
              , [ 0, 1 ] 
              , [ 0, 2 ]
              , [ 0, 3 ]
              , [ 0, 4 ]
              , [ 0, 5 ]
              , [ 0, 6 ]]

funs = [ "add", "subtract", "project", "reject", "unit", "scale", "rotate", "trace"]
meta = [ "save", "undo", "redo", "clear", "set", "reset", "yes", "no"]
defs = [ "An atom is the value behind a variable."
       , "A span is the set of all combinations of a vector or family of vectors"
       , "A projection is the component of a vector parallel to another vector"
       , "A rejection is the component of a vector perpendicular to another vector" 
       ]
vars = ["a", "b", "c", "d" ]--, "e", "f", "g" {-, "+", "-" -}]

-- ids
restState = 0
hoverState = 1
clickState = 2
hideState = 3
