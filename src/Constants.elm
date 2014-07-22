module Constants where

import Array as A

import Expr as E
import Vector as V


{-
    TODO:
    add +/- to vars?
-}

data State = Available | Hidden
data Button = Fun Int State | Var Int State | Meta Int State
data Action = Click | Hover | None

type SharedModel a = { a
                     | expr   : E.Expr
                     , value  : V.Space
                     , exprs  : [E.Expr]
                     , values : [V.Space]
                     }
type UIModel a = { a
                 | funs    : A.Array Int
                 , vars    : A.Array Int
                 , meta    : A.Array Int
                 , index   : Int
                 , history : [Button]
                 , base    : [Button]
                 }
type GraphModel a = { a
                    | basis : [(Float,Float)]
                    , units : Float 
                    , theta : Float
                    }
type Model = (UIModel (GraphModel (SharedModel {})))

model : Model
model = { 
        -- shared
        expr = E.Empty
        , value = V.Abyss
        , exprs = expressions -- list of var expr
        , values = values -- list of vectors behind all expressions
        -- ui part 
        , funs = A.repeat 8 0
        , vars = A.fromList [0,0,0,0,3,3,3] 
        , meta = A.fromList [2,2,2,2]
        , index = 0 -- index of expr
        , history = [] -- buffer of buttons which can be undone
        , base = [] -- buffer of commited changes
        -- graph part
        , basis  = basis
        , units = 1
        , theta = 0 -- determines rotation and scaling
        }

units = 1.0
basis  = [(0,40),(40,0),(0,40)]
historyLimit = 20
velocity = pi/10
omega = pi/10
fps = 700
values = [ V.Vector 1 1 1
         , V.Vector -1 2 4
         , V.Vector -1 -1 4 
         , V.Vector 1 -1 2 ]

expressions = [ E.Val 0
              , E.Val 1
              , E.Val 2 
              , E.Val 3 ]

funs = [ "add", "subtract", "project", "reject", "unit", "negate", "scale", "rotate"]--, "trace"]
meta = [ "save", "undo", "redo", "clear"]
defs = [ "add a b - returns the sum of a and b"
       , "subtract a b - returns the difference of a and b" 
       , "project a b - returns the projection of a onto b\nthe projection is the component of the first which is parallel to the second"
       , "reject a b - returns the rejection of a onto b \nthe rejection is the component of the first which is perpendicualar to the second"
       , "unit a - returns normalized a\nthe normal of a vector lies in the same direction with a length of 1"
       , "negate a - returns a scaled by -1"
--     , "scale a - returns all vectors in the family of a with length from 1/2 to 2"
--     , "rotate a b - returns all vectors a rotated about b" 
--     , "trace a - returns the path a vector makes, useful for visualizing rotates, scales"
       , "save - saves the current expression as a new variable" 
       , "undo - undoes the last change"
       , "redo - seriously?" 
       , "clear - clears the current expression"
       , "? - provides info about any button"
       ]
vars = ["a", "b", "c", "d" , "e", "f", "g" {-, "+", "-" -}]
colors = [blue, purple, red, green, orange, lightBlue, lightPurple, lightRed, lightGreen,lightOrange]


