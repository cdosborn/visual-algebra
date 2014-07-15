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

main = render <~ graph ~ ui

-- Model
--defined in Constants as C.model

-- Update
ui = Ui.render <~ Window.dimensions ~ Ui.state
graph = Graph.render <~ Window.dimensions 
                      ~ (addUiStateToGraph <~ Ui.state 
                                            ~ Graph.state)

addUiStateToGraph u g = { g | values <- u.values, expr <- u.expr, exprs <- u.exprs }
            

-- Render
render : Element -> Element -> Element
render g u = 
     layers [ g, u ]


