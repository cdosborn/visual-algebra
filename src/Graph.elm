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
--defined in Constants as C.model

-- Update
update : Inputs -> C.Model -> C.Model
update s model = 
    let delta = s
        b1 = head model.basis 
        b2 = head (tail model.basis)
        b3 = head (tail (tail model.basis))
        b1' = V.rotate' b1 (C.velocity * delta / 1000)
        b2' =  b2
        b3' = V.rotate' b3 (C.velocity * delta / 1000)
    in { model | basis <- [b1',b2',b3'] }


-- Render
render (w, h) model = 
    let basis = model.basis
        axis = [ (V.Vector 1 0 0, greyscale 0.3)
               , (V.Vector 0 1 0, greyscale 0.3)
               , (V.Vector 0 0 1, greyscale 0.3)
               ]
        spaces = model.values-- ++ expr
        colored = map (\s -> (s, blue)) spaces
        -- sortGeoms when geoms change
        forms = map (\(s, col) -> (V.draw basis model.units col s)) (colored ++ axis)
        grid = V.drawGrid basis model.units
        style = (\str -> (leftAligned (T.height 12 (monospace (toText str)))))
        moves = model.values
----        --|> map (\i -> head (drop i model.values)) --map to Vectors
            |> map (\v -> 
                case v of
                V.Vector a b c -> [a,b,c]) --map to list of components 
            |> map (\v -> foldr V.add' (0,0) (zipWith (\c b -> V.scale' b c) v basis)) --zip to pair of coord
        textForms = map (\i -> toForm (style (head (drop i C.vars)))) [0 .. ((length model.values) - 1)]
        shiftThem = (\(x,y) frm -> move (x, y + 13) frm)
        texts =  zipWith  shiftThem moves textForms
        allForms = grid ++ texts ++ forms
    in collage w h allForms

-- Signals

--durp = lift5 (\a b c d e ->{a=a,b=b,c=c,d=d,e=e}) Mouse.isDown Mouse.position Window.dimensions randColors (fps frameSpeed)
--time = sampleOn durp (every millisecond)
--
type Inputs = Time
signals : Signal Inputs
signals = fps C.fps
--
--
--randColorSeed = Random.float (constant 4)
--getRandColors seed =
--    let many = 5
--        start = seed * 100
--        delta = pi / (20 * (toFloat many))
--    in map (\i -> hsl (start + delta * (toFloat i)) 1 0.3) [1 .. many]
--
--randColors : Signal [Color]
--randColors = lift getRandColors randColorSeed
--main = lift2 render signals (foldp brain model signals)
--
