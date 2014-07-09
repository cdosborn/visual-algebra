import Window
import Mouse
import Either

import Constants as C
import Ui
import Graph
import Debug (..)

{-
    TODO:
    evalSpan handles span of no arguments should just return an abyss
    handling functions with incorrect arguments..
    correct sorting of geoms
    visual-algebra intro screen with vowels floating around
    color-coordination between vector and button highlight
    2d/3d mode with or w/o rotation

    Philosophy:
    Each main module defines a model and accompanying methods (update,render,signals) based on that model.
    These models provide an interface for a record that can be passed to them. 
-}

main = render <~ Window.dimensions ~ state

-- Model
--defined in Constants as C.model

-- Update
update signal m =
    case signal of
    (Either.Left a) -> Graph.update a m
    (Either.Right b) -> Ui.update b m

-- Render
render (w,h) m = 
     layers [ Graph.render (w,h) m 
            , Ui.render (w,h) m ]

-- Signals
--delta = fps C.fps
state = foldp update C.model signals

signals : Signal (Either.Either {a:Bool,b:(Int,Int),c:(Int,Int)} (Ui.Action, Ui.Button))
signals = merge (Either.Left <~ Graph.signals) (Either.Right <~ Ui.signals)
