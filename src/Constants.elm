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
        , meta = (A.fromList [2,2,2,2,0])-- transparent state
        , index = 0 -- index of expr
        , history = [] -- buffer of buttons which can be undone
        , base = [] -- buffer of commited changes
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

funs = [ "add", "subtract", "project", "reject", "unit", "scale", "rotate", "trace"]
meta = [ "save", "undo", "redo", "clear", "?"]
defs = [ "An atom is the value behind a variable."
       , "A span is the set of all combinations of a vector or family of vectors"
       , "A projection is the component of a vector parallel to another vector"
       , "A rejection is the component of a vector perpendicular to another vector" 
       ]
vars = ["a", "b", "c", "d" , "e", "f", "g" {-, "+", "-" -}]
colors = [blue, purple, red, orange, lightBlue, lightPurple, lightRed, lightOrange]

-- ids
restState = 0
hoverState = 1
clickState = 2
hideState = 3
