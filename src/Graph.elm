module Graph where

import Window
import Mouse 
import Text as T
import Array as A
import Time (fps,Time)

import Constants as C
import Drag
import Expr as E
import Vector as V
import Debug (log)

{-
    TODO:
    
    update: 
    conversion fun, expr -> space    
    eval converted space
    if possible/theres a good way to limit cost of both graph and ui events:
        graph should always rotate, and should respond to drag, which
        affects graph rotation (skewing the graph is rarely helpful)

    render:
    show rotation arrow if C.rotate
    glow effect on axis and click mouse icon on hover?
    update draw

    grammar:

-}

-- Render
render : (Int, Int) -> {value:V.Space,values:[V.Space],expr:E.Expr,time:Time} -> Element
render (w, h) {value, values, expr, time} =
    let b1 = head C.basis 
        b2 = head (tail C.basis)
        b3 = head (tail (tail C.basis))
        b1' = V.rotate' b1 (C.velocity * time / 1000)
        b2' = V.rotate' b2 (C.velocity * time / 1000)
        b3' =  b3
        basis = [b1',b2',b3']
        axis = [ (V.Vector 1 0 0, greyscale 0.3, Nothing) -- (value, color, maybe label)
               , (V.Vector 0 1 0, greyscale 0.3, Nothing)
               , (V.Vector 0 0 1, greyscale 0.3, Nothing)
               ]
        theta = C.omega * time / 1000
        values' = map (V.eval theta) values
        value'  = V.eval theta value
        len = length values'
        getSpaces = (\ids -> map (\i -> (head (drop i values'), head (drop i C.colors), Just (head (drop i C.vars)))) ids)
        allSpaces = if value' == V.Abyss 
                    then getSpaces [0 .. (len - 1)]
                    else [(value', (head (drop len C.colors)), Nothing)] ++ (getSpaces (E.getDependencies expr))
        forms = map (\(s, col, label) -> (V.draw basis C.units label col s)) (axis ++ allSpaces)
        grid = V.drawGrid basis C.units
        allForms = grid ++ forms
    in collage w h allForms

