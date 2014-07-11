import Window
import Mouse
import Either

import Constants as C
import Ui
import Graph
import Debug (..)

{-
    Philosophy:
    Each module shares the same model but defines (update,render,signals) based on 
    what it needs from the model.

    TODO:
    evalSpan handles span of no arguments should just return an abyss
    handling functions with incorrect arguments..
    correct sorting of geoms
    visual-algebra intro screen with vowels floating around
    color-coordination between vector and button highlight
    2d/3d mode with or w/o rotation

-}

main = render <~ Window.dimensions ~ state

-- Model
--defined in Constants as C.model

-- Update
update : Inputs -> C.Model -> C.Model
update (g,u) m =
    if u == m.oldUiInput 
    then Graph.update g m
    else Graph.update g <| Ui.update u m
            

-- Render
render : (Int, Int) -> C.Model -> Element
render (w,h) m = 
     layers [ Graph.render (w,h) m 
            , Ui.render (w,h) m ]

-- Signals
--delta = fps C.fps
state : Signal C.Model
state = Graph.state--foldp update C.model signals
-- .expr <~ Ui.state


type Inputs = (Graph.Inputs, Ui.Inputs) 
signals : Signal Inputs
signals = (,) <~ Graph.signals ~ Ui.signals
