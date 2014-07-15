module Drag where
import Mouse
import Window

main : Signal Element
main = asText <~ state

-- state (isDown, pos of first click, pos relative to start)
type Drag = (Bool,(Int,Int),(Int,Int))
state : Signal Drag
state = (,,) <~ Mouse.isDown ~ mouseFirstDown ~ relativeToStart

-- Signal of the position vector relative to beginning of drag
relativeToStart = subt <~ mouseFirstDown 
                        ~ merge posOnDown (keepWhen Mouse.isDown (0,0) Mouse.position)

-- Position of click with respect to center of window
mouseFirstDown = center <~ Window.dimensions 
                         ~ keepWhen Mouse.isDown (0,0) posOnDown

-- Position of mouse down
posOnDown = sampleOn Mouse.isDown Mouse.position

-- The difference of two vectors
subt vec vec2 = (-1 * ((fst vec) - (fst vec2)), (snd vec) - (snd vec2))

-- Adjusts coordinates with respect to center of window
center (w,h) p = 
    let x = (fst p) - (div w 2)
        y = -(snd p) + (div h 2)
    in (x, y)
