import Mouse
import Window
import Graphics.Input (Input, input, clickable)
import Graphics.Input.Field as Field
import Vector as V
import Text as T
import Random

--DATA:
model = { basis = [(50,50),(0,50),(50,0)]
        , geoms = [ V.Atom (V.Vector 2 2 2) 
                  , V.Atom (V.Plane [V.Vector 1 1 0, V.Vector 1 0 1])
                  , V.Atom (V.Volume)
                  ]
        , forms = []
        , colors = []
        , velocity = pi/60
        , frameRate = 60
        , units = 2}

--Brain
brain updates model = 
    let colors = updates.b
        delta = updates.c
--      b1 = if | target == "" -> V.rotate' b1' (velocity * delta / 1000)
--              | otherwise -> b1'
--      b3 = if | target == "" -> V.rotate' b3' (velocity * delta / 1000)
--              | otherwise -> b3'
--      
--      basis = [b1,b2',b3]
        
        forms = if (not (model.forms == []))
                then model.forms
                else 
                let basis = model.basis
                    axis = [ (V.Atom(V.Vector 1 0 0), (greyscale 0.3))
                           , (V.Atom(V.Vector 0 1 0), (greyscale 0.3))
                           , (V.Atom(V.Vector 0 0 1), (greyscale 0.3))
                           ]
                    geoms = zip model.geoms colors
                    -- sortGeoms when geoms change
                    allGeoms = V.sortGeoms (axis ++ geoms)
                    spaces = map (\(geom, col) -> (V.draw basis model.units col (V.eval geom))) allGeoms
                    grid = V.drawGrid basis model.units
                in grid ++ spaces
        forms' = map (rotate (pi/100)) forms
    in { model | colors <- colors 
               , forms <- forms' }
                     

--RENDER
render updates model = 
    let window = updates.a
        width = toFloat (fst window)
        height = toFloat (snd window)
    in layers [ collage (round width) (round height) model.forms
              , ui window
              ]

--SIGNALS
signals : Signal {a:(Int,Int),b:[Color],c:Time}
signals = lift3 (\a b c->{a=a,b=b,c=c}) Window.dimensions randColors (fps model.frameRate)

ui (w, h) = 
    let x = 4
    in container 600 h midTop (flow down [ 
          (spacer 250 150) 
        , (leftAligned (T.height 30 (monospace (toText "visual-algebra"))))
--      , (link "begin.elm" (leftAligned (line Under (monospace (toText "begin")))))
--      , (link "about.elm" (leftAligned (line Under (monospace (toText "about")))))
     ])

randColorSeed = Random.float (constant 4)
getRandColors seed =
    let many = length model.geoms
        start = seed * 100
        delta = pi / (30 * (toFloat many))
    in map (\i -> hsl (start + delta * (toFloat i)) 1 0.3) [1 .. many]

randColors : Signal [Color]
randColors = lift getRandColors randColorSeed

main = lift2 render signals (foldp brain model signals)
