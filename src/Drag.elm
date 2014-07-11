module Drag where
import Mouse

main : Signal Element
main = asText <~ state

--state : Signal (Bool,(Int,Int))
--state = merge ((\point -> (False,point)) <~ mouseDragStop) 
--              ((\point -> (True,point)) <~ mouseDragStart) 
--
mouseDragStart = keepWhen Mouse.isDown (0,0) posOnDown
mouseDragStop = dropWhen Mouse.isDown (0,0) posOnDown

posOnDown : Signal (Int, Int)
posOnDown = (sampleOn Mouse.isDown Mouse.position)

state = subt <~ posOnDown ~ (merge posOnDown (keepWhen Mouse.isDown (0,0) Mouse.position))

subt vec vec2 = (-1 * ((fst vec) - (fst vec2)), (snd vec) - (snd vec2))
