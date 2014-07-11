module Drag where
import Mouse

main : Signal Element
main = asText <~ state

-- state (isDown, pos of first click, pos of current relative to start)
state : Signal (Bool,(Int,Int),(Int,Int))
state = (,,) <~ Mouse.isDown ~ mouseFirstDown ~ relativeToStart

-- Signal of the position vector relative to beginning of drag
relativeToStart = subt <~ mouseFirstDown ~ (merge posOnDown (keepWhen Mouse.isDown (0,0) Mouse.position))

mouseFirstDown = keepWhen Mouse.isDown (0,0) posOnDown
posOnDown = sampleOn Mouse.isDown Mouse.position


subt vec vec2 = (-1 * ((fst vec) - (fst vec2)), (snd vec) - (snd vec2))
