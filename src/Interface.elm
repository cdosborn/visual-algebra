module Interface where
import Vector as V
import Window
import Text as T
import Graphics.Input (..)

{-
    TODO:
    pull state out of the selected signal
    expose only signals and ui method

-}


ui window selected = 
    let w = fst window 
        h = snd window 
        vs = [0 .. ((length vecNames) - 1)]
        geoms = [0 .. ((length geomNames) - 1)]
        chosenVs = sort selected.vectors
        chosenGeoms = selected.geoms
        vecButtons = buildButtons vecInput' vs chosenVs 
        geomButtons = buildButtons geomInput' geoms chosenGeoms
    in container 600 h middle (flow down 
        [ (leftAligned (T.height 30 (monospace (toText "visual-algebra"))))
        , (leftAligned (T.height 10 (monospace (toText "v0.01 E-5 alpha"))))
        , spacer 250 10
        , container 250 100 topLeft
            (flow down 
                [ (flow right vecButtons)])
        , container 250 500 topLeft 
            (flow down 
                [ (flow right geomButtons)])

     ])

geomNames = ["atom", "span", "project", "reject"]
vecNames = ["a", "b", "c", "d", "e"]
vecSignal = (foldp swap [0] vecInput.signal)
selected = lift2 (\a b -> {geoms=[a], vectors=b}) geomInput.signal vecSignal

--limitVector a b = 
--    let len = length b 
--        b' = case a of 
--            2 -> if len > 2 then take 2 b else b
--            3 -> if len > 2 then take 2 b else b 
--            _ -> b
--        extra = if len > 2 then drop 2 b else []
--    in {geoms=[a], vectors=b', unallowed=extra}

-- Pre: assumes both new and old are number lists
-- Post: returns a list, which swaps the state as to whether old contained new
swap : Int -> [Int] -> [Int]
swap new old = 
    case old of
     [] -> [new]
     one::more -> 
         if new == one
         then more
         else one::(swap new more)

geomInput : Input Int
geomInput = input 0

vecInput : Input Int
vecInput = input 0

geomInput' = {geomInput | name = "geom"}
vecInput' = {vecInput | name = "vector"}

buildButtons inputType buttonIndexes chosen = 
    case buttonIndexes of
    [] -> []
    b::bs -> 
        case chosen of
        [] -> (regButton inputType b)::(buildButtons inputType bs chosen)
        c::cs -> if c == b
                 then (styleButton inputType b)::(buildButtons inputType bs cs)
                 else (regButton inputType b)::(buildButtons inputType bs chosen)

styleButton inputType index =
    let name = if (inputType.name == "vector")
               then head (drop index vecNames) 
               else head (drop index geomNames)
        element = (leftAligned (T.height 15 (monospace (line Under (toText name)))))
        w = widthOf element
        h = heightOf element
        linked = (color (rgba 0 0 0 0.2) (link "#" (container (w + 15) (h + 10) middle element)))
    in clickable inputType.handle index linked

regButton inputType index =
    let name = if (inputType.name == "vector")
               then head (drop index vecNames) 
               else head (drop index geomNames)
        element = (leftAligned (T.height 15 (monospace (toText name))))
        w = widthOf element
        h = heightOf element
        linked = (link "#" (container (w + 15) (h + 10) middle element))
    in clickable inputType.handle index linked
