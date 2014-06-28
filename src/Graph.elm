module Graph where

import Window.dimensions
import Mouse.position
import Vector as V

-- Model
model = { basis  = [(0,40),(0,50),(50,0)]
        , target = "" 
        , units = 1
        , time = 0
        , velocity = pi/20 }

-- Update
update signals model =
    let pressed = updates.a
        mouse = updates.b       
        window = updates.c
        delta = updates.f - model.time
        selected = updates.g
        vs = map (\i -> head (drop i vectors)) selected.vectors
        geoms = let index = (head selected.geoms)
                    name = head (drop index geomNames)
                    def = map (\v -> V.Atom v) vs 
                in case name of
                    "atom" -> def
                    "span" -> [V.Span vs] ++ def
                    "project" -> if (length vs) == 2
                                 then (V.Project (head vs) (head (tail vs)))::(head def)::[(head (tail def))]
                                 else [V.Atom V.Abyss]
                    "reject" -> if (length vs) == 2
                                 then (V.Reject (head vs) (head (tail vs)))::(head def)::[(head (tail def))]
                                 else [V.Atom V.Abyss]
                    _ -> [V.Atom V.Abyss]
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
        
        basis = [b1,b2,b3]
    in {basis=basis, geoms=geoms,velocity=model.velocity, units=model.units, time=updates.f, target=target} 


-- Render
render w h model = 
    lek 
        mouse = updates.b       
        window = updates.c
        selected = updates.g
        width = toFloat (fst window)
        height = toFloat (snd window)
        basis = model.basis
        axis = [ (V.Atom(V.Vector 1 0 0), (greyscale 0.3))
               , (V.Atom(V.Vector 0 1 0), (greyscale 0.3))
               , (V.Atom(V.Vector 0 0 1), (greyscale 0.3))
               ]
        --geoms = zip model.geoms colors
        geoms = map (\g -> (g, blue)) model.geoms 
        -- sortGeoms when geoms change
        allGeoms = V.sortGeoms (axis ++ geoms)
        spaces = map (\(geom, col) -> (V.draw basis model.units col (V.eval geom))) allGeoms
        grid = V.drawGrid basis model.units
        style = (\str -> (leftAligned (T.height 12 (monospace (toText str)))))
        moves = selected.vectors 
            |> map (\i -> head (drop i vectors)) --map to Vectors
            |> map (\v -> case v of
                          V.Vector a b c -> [a,b,c]) --map to list of components 
            |> map (\v -> foldr V.add' (0,0) (zipWith (\c b -> V.scale' b c) v basis)) --zip to pair of coord
        textForms = map (\i -> toForm (style (head (drop i vecNames)))) selected.vectors
        shiftThem = (\(x,y) form -> move (x, y + 13) form)
        texts =  zipWith  shiftThem moves textForms
        allForms = spaces ++ grid ++ texts
    in layers [ collage (round width) (round height) allForms
              , asText model
              , ui window selected
              ]

-- Signals

--durp = lift5 (\a b c d e ->{a=a,b=b,c=c,d=d,e=e}) Mouse.isDown Mouse.position Window.dimensions randColors (fps frameSpeed)
--time = sampleOn durp (every millisecond)
--
----signals : Signal {a:Bool,b:(Int,Int),c:(Int,Int),d:[Color],e:Time, f:Time}
--signals = lift7 (\a b c d e f g->{a=a,b=b,c=c,d=d,e=e,f=f,g=g}) Mouse.isDown Mouse.position Window.dimensions randColors (fps 20) time selected
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

