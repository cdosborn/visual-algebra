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
                 , info    : String
                 }
type GraphModel a = { a
                    | basis : [(Float,Float)]
                    , units : Float 
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
        , funs = (A.repeat (length funs) 0)
        , vars = (A.fromList [0,0,0,0,3,3,3]) --  transparent state
        , meta = (A.fromList [2,2,2,2])-- transparent state
        , index = 0 -- index of expr
        , history = [] -- buffer of buttons which can be undone
        , base = [] -- buffer of commited changes
        , info = ""
        -- graph part
        , basis  = [(0,40),(0,50),(50,0)]
        , units = 1
        }

historyLimit = 20
velocity = pi/40
fps = 30
values = [ V.Vector 1 1 1
         , V.Vector 2 1 2
         , V.Vector 3 3 0
         , V.Vector 1 0 2 ]

expressions = [ E.Leaf 0
              , E.Leaf 1
              , E.Leaf 2
              , E.Leaf 3 ]

funs = [ "add", "subtract", "project", "reject", "unit"]--, "scale", "rotate", "trace"]
meta = [ "save", "undo", "redo", "clear"] --, "?"]
defs = [ "add a b - returns the sum of a and b"
       , "subtract a b - returns the difference of a and b" 
       , "project a b - returns the projection of a onto b\n\nthe projection is the component of the first which is parallel to the second"
       , "reject a b - returns the rejection of a onto b \n\nthe rejection is the component of the first which is perpendicualar to the second"
       , "unit a - returns normalized a\n\nthe normal of a vector lies in the same direction with a length of 1"
       , "scale a - returns all vectors in the family of a with length from 1/2 to 2"
       , "rotate a b - returns all vectors a rotated about b" 
       , "trace a - returns the path a vector makes, useful for visualizing rotates, scales"
       , "save - saves the current expression as a new variable" 
       , "undo - undoes the last change"
       , "redo - seriously?" 
       , "clear - clears the current expression"
       , "? - provides info about any button"
       ]
vars = ["a", "b", "c", "d" , "e", "f", "g" {-, "+", "-" -}]
colors = [blue, purple, red, green, orange, lightBlue, lightPurple, lightRed, lightGreen,lightOrange]

-- ids
restState = 0
hoverState = 1
clickState = 2
hideState = 3
