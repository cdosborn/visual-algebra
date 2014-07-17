module Constants where

import Array as A

import Expr as E
import Vector as V
import Debug (log)


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
        , funs = (A.fromList [0,0,0,0,0,0,0,0])
        , vars = (A.fromList [0,0,0,0,3,3,3])
        , meta = (A.fromList [2,2,2,2])
        , index = 0 -- index of expr
        , history = [] -- buffer of buttons which can be undone
        , base = [] -- buffer of commited changes
        -- graph part
        , basis  = [(0,40),(0,40),(40,0)]
        , units = 1
        , theta = 0 -- determines rotation and scaling
        }

historyLimit = 20
velocity = pi/40
fps = 30
values = [ V.Vector 1 1 1
         , V.Vector 2 1 2
         , V.Vector 0.4 0.6 0
         , V.Vector 1 0 2 ]

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

-- ids
restState = 0
hoverState = 1
clickState = 2
hideState = 3

exprToSpace expr exprs values =
    case expr of
    E.Empty -> V.Abyss
    E.Val valId -> head (drop valId values)
    E.Ref varId -> exprToSpace (head (drop varId exprs)) exprs values
--  E.Ref varId -> head (drop varId values)
    E.Unary funId e -> let s = exprToSpace e exprs values in
        if | funId == 4 -> V.Unit s
           | funId == 5 -> V.Negate s
           | funId == 6 -> V.Scale s
           | funId == 8 -> V.Trace s
    E.Duo funId a b -> 
        let s1 = exprToSpace a exprs values
            s2 = exprToSpace b exprs values
        in if | funId == 0 -> V.Add s1 s2
              | funId == 1 -> V.Subtract s1 s2
              | funId == 2 -> V.Project s1 s2
              | funId == 3 -> V.Reject s1 s2
              | funId == 7 -> V.Rotate s1 s2
