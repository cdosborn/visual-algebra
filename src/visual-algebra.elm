import Window
import Mouse
import Either
import Time

import Constants as C
import Ui
import Graph
import Debug (log)

{-
    Philosophy:
    Each module shares the same model but defines (update,render,signals) based on 
    what it needs from the model.

    TODO:
    correct sorting of geoms
    visual-algebra intro screen with vowels floating around
    color-coordination between vector and button highlight
    2d/3d mode with or w/o rotation

-}

-- Display app 
main = scene <~ graph ~ ui

-- Draw each module, communicate between graph/ui
ui = Ui.render <~ Window.dimensions 
                ~ Ui.state

graph = Graph.render <~ Window.dimensions 
                      ~ sampleOn rate graphS

graphS = graphFromUi <~ Ui.state ~ totalTime
totalTime = foldp (+) 0 rate
rate = fps C.fps

graphFromUi u time = 
    { value=u.value
    , values=u.values
    , expr=u.expr
    , time = time }

-- Combine drawings
scene : Element -> Element -> Element
scene g u = 
     layers [ g, u ]


