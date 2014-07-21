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


main = render <~ Window.dimensions ~ state
state = foldp update C.model signals

-- Model

model = { delta = 0
        , space = V.Abyss
        , spaces = [V.Abyss]
        , basis  = [(0,40),(40,0),(0,40)] }
--defined in Constants as C.model
--filter : C.Model -> {expr:E.Expr, exprs:[E.Expr]}
--filter m = 
--    { expr <- u.expr
--    , exprs <- u.exprs 
--    , mexpr <- mexpr
--    , mexprs <- mexprs
--    }
--

-- Update
update : Inputs -> C.Model -> C.Model
update s model = 
    let delta = s
        b1 = head model.basis 
        b2 = head (tail model.basis)
        b3 = head (tail (tail model.basis))
        b1' = V.rotate' b1 (C.velocity * delta / 1000)
        b2' = V.rotate' b2 (C.velocity * delta / 1000)
        b3' =  b3
        theta = model.theta + (C.omega * delta / 1000)
    in { model | basis <- [b1',b2',b3'] 
       , theta <- theta }

mutate theta exprs values = mutateHelper theta exprs values (length C.values)

mutateHelper theta exprs values index =
    if index == (length exprs)
    then values
    else 
        let e = head (drop index exprs)
            v = V.mEval theta (C.exprToSpace e exprs values)
        in mutateHelper theta exprs (values++[v]) (index + 1)

-- Render
render (w, h) model = 
    let basis = model.basis
        axis = [ (V.Vector 1 0 0, greyscale 0.3, Nothing) -- (value, color, maybe label)
               , (V.Vector 0 1 0, greyscale 0.3, Nothing)
               , (V.Vector 0 0 1, greyscale 0.3, Nothing)
               ]
        theta = model.theta
        exprs = model.exprs
        expr = model.expr
        values = mutate theta exprs C.values
        value  = V.mEval theta (C.exprToSpace expr exprs values)
        getSpaces = (\ids -> map (\i -> (head (drop i values), head (drop i C.colors), Just (head (drop i C.vars)))) ids)
        allSpaces = if value == V.Abyss 
                    then getSpaces [0 .. ((length exprs) - 1)]
                    else [(value, (head (drop (length exprs) C.colors)), Nothing)] ++ (getSpaces (E.getDependencies expr))
        sorted = (axis ++ allSpaces)--V.sortSpaces theta (axis ++ allSpaces)
        forms = map (\(s, col, label) -> (V.draw basis model.units label col s)) sorted
        grid = V.drawGrid basis model.units
--      msg = [toForm (asText theta)]
        allForms = grid ++ forms --++ msg
    in collage w h allForms

-- Signals

--durp = lift5 (\a b c d e ->{a=a,b=b,c=c,d=d,e=e}) Mouse.isDown Mouse.position Window.dimensions randColors (fps frameSpeed)
--time = sampleOn durp (every millisecond)
--
type Inputs = Time
signals : Signal Inputs
signals = fps C.fps
