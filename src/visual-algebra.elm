import Window
import Graph
import Ui
import Constants as C

{-
    TODO:
    add interfaces in modulels for what type of records they handle?
    OMG FIX RENDER TO ONLY WRITE TEXT FROM THE BRAIN
    evalSpan handles span of no arguments should just return an abyss
    handling functions with incorrect arguments..
    drawing of planes/volumes (planes should just be fixed leg, based on direction
    sorrect sorting of geoms
    visual-algebra intro screen with vowels floating around!!!!
    color-coordination between vector and button highlight
    2d/3d mode with or w/o rotation

    Philosophy:
    Each main module defines a model and accompanying methods (update,render,signals) based on that model.
    These models provide an interface for a record that can be passed to them. Unfortunately there is no
    dynamic way to add each record field to the main model.
-}

main = render <~ Window.dimensions ~ (sampleOn delta state)

-- Model
model = { basis = Graph.basis
        , target = Graph.target
        , units = Graph.units
        , time = Graph.time    --delete?
        , velocity = Graph.velocity
        , geoms = Ui.geoms
        , funs = Ui.funs
        , vars = Ui.vars
        , varDefs = Ui.varDefs
        , funDefs = Ui.funDefs
        , selected = Ui.selected
        , notAllowed = Ui.notAllowed }

-- Update
update (g,u) model = 
    let result = Ui.update u model
        --communication betw ui / graph
    in Graph.update g result

-- Render
render (w,h) m = 
    layers [ model.graph.render w h m
           , model.ui.render w h m ]

-- Signals
delta = fps C.fps
state = foldp update model signals
signals = lift2 (,) Graph.signals Ui.signals
