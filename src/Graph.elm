module Graph where

import Window
import Mouse 
import Text as T

import Constants as C
import Expr as E
import Vector as V

{-
    TODO:
    
    conversion fun, expr -> space    
    eval converted space
    update draw
    grammar:

-}


main = render <~ Window.dimensions ~ (foldp update model signals)

-- Model
model = { basis  = [(0,40),(0,50),(50,0)]
        , target = "" 
        , units = 1
        , values = 
         [ V.Vector 1 1 1
         , V.Vector 2 1 2
         , V.Vector 3 3 0
         , V.Vector 1 0 2 ]
        , selected = [0]
        , tempValue = V.Vector 1 1 1
        , tempExp = [0, 0]
        , time = 0
        , velocity = pi/20 }

--model = { exprs = C.expressions -- [[functionID, varID,..]], list of var expr
--        , values = C.values -- list of vectors behind all expressions
--        , temp = [] -- [[functionID, varID , ...]]
--        , funs = A.repeat (length C.funs) 0
--        , vars = A.repeat (length C.vars) 0 --  transparent state
--        , meta = A.repeat (length C.meta) 2 -- transparent state
--        , index = 0 -- index of expr
--        , expr = E.Empty 
--        }

-- Update
update signals model =
    let pressed = signals.a
        mouse = signals.b       
        window = signals.c
        delta = signals.d
        x' = toFloat ((fst mouse) - (div (fst window) 2))
        y' = toFloat (-(snd mouse) + (div (snd window) 2))
        b1' = head model.basis 
        b2' = head (tail model.basis)
        b3' = head (tail (tail model.basis))
        near = filter (\v -> (V.distance' v (x', y')) < 10) [b1', b2', b3']
        target = if | not pressed -> ""
                    | not (model.target == "") -> model.target
                    | near == [] -> ""
                    | otherwise -> let closest = head (sortBy (\v -> V.distance' v (x', y')) near)
                                   in if | b1' == closest -> "b1"
                                         | b2' == closest -> "b2"
                                         | otherwise  -> "b3"
        b1 = if | target == "b1" -> (x',y') 
                | target == "" -> V.rotate' b1' (model.velocity * delta / 1000)
                | otherwise -> b1'
        b2 = if | target == "b2" -> (x',y') 
                | otherwise -> b2'
        b3 = if | target == "b3" -> (x',y') 
                | target == "" -> V.rotate' b3' (model.velocity * delta / 1000)
                | otherwise -> b3'
    in { model | basis <- [b1,b2,b3]
               , target <- target }


-- Render
render (w, h) model = 
    let basis = model.basis
        axis = [ (V.Atom(V.Vector 1 0 0), (greyscale 0.3))
               , (V.Atom(V.Vector 0 1 0), (greyscale 0.3))
               , (V.Atom(V.Vector 0 0 1), (greyscale 0.3))
               ]
        spaces = model.values ++ expr
        colored = map (\s -> (s, blue)) spaces
        -- sortGeoms when geoms change
        forms = map (\(s, col) -> (V.draw basis model.units col s)) spaces
        grid = V.drawGrid basis model.units
        style = (\str -> (leftAligned (T.height 12 (monospace (toText str)))))
        moves = model.selected
            |> map (\i -> head (drop i model.values)) --map to Vectors
            |> map (\v -> case v of
                          V.Vector a b c -> [a,b,c]) --map to list of components 
            |> map (\v -> foldr V.add' (0,0) (zipWith (\c b -> V.scale' b c) v basis)) --zip to pair of coord
        textForms = map (\i -> toForm (style (head (drop i C.vars)))) model.selected
        shiftThem = (\(x,y) form -> move (x, y + 13) form)
        texts =  zipWith  shiftThem moves textForms
        allForms = spaces ++ grid ++ texts
    in layers [ collage (round width) (round height) allForms
              , asText model
              ]

-- Signals

--durp = lift5 (\a b c d e ->{a=a,b=b,c=c,d=d,e=e}) Mouse.isDown Mouse.position Window.dimensions randColors (fps frameSpeed)
--time = sampleOn durp (every millisecond)
--
signals : Signal {a:Bool,b:(Int,Int),c:(Int,Int),d:Time}
signals = lift4 (\a b c d->{a=a,b=b,c=c,d=d}) Mouse.isDown Mouse.position Window.dimensions (fps 20)
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

