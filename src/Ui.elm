module Ui where

import Vector as V
import Constants as C
import Window
import Text as T
import Graphics.Input (..)

{-
    TODO:
    
    FIX TEMPORARY


    in render does V.Span [Abyss] cause errors
    add custom button input for + - to enable hover/etc
    Why does selected have to be sorted in render?? -- because of button creation makes it way more efficient
        maybe throw sorting inside of getButtons, to remove confusion
    **when updating geometry, often i have pre-evaluated code, aka current geom
    extract similar code in update methods (eval geom part) (eval geom part)
    maybe i could write evaluate to take existing geom :/
    pull state out of the selected signal
    expose only signals and ui method
    pull out all 7s in render, add to Constants
    
    grammar:
    exprs : expressions
    temp(Value|Exp) : represents the evaluated geometry before being stored as a value
    *Update : helper methods which handle * type of input (variable button/plus button)

-}

main = render <~ Window.dimensions ~ (foldp update model signals)

-- Model
model = { expressions = 
             [ [ 0, 0 ] -- [functionID, varID,..]  
             , [ 0, 1 ] 
             , [ 0, 2 ]
             , [ 0, 3 ] ]
        , values = 
             [ V.Vector 1 1 1
             , V.Vector 2 1 2
             , V.Vector 3 3 0
             , V.Vector 1 0 2 ]
        , tempValue = V.Abyss
        , tempExp = [0]
        , geomSelected = 0 -- id of geom
        , selected = []
        , notAllowed = [] }

-- Update
-- interpret selected into buttons pressed
-- for fun pressed determine selected/notallowed
-- package results into a model
update signal model =
    let inputId = fst signal
        inputType = snd signal
    in if | inputType == "variable" -> varUpdate inputId model
          | inputType == "geom" -> geomUpdate inputId model
          | inputType == "custom" -> customUpdate inputId model

varUpdate inputId model =
    let geomId = model.geomSelected
        old = model.selected ++ model.notAllowed
        toggled = toggleList inputId old
        selected = if | geomId == 0 -> take 1 toggled
                      | geomId == 2 || geomId == 3 -> take 2 toggled
                      | otherwise -> toggled
        notAllowed = drop (length selected) toggled
        args = map (\i -> head (drop i model.values)) selected
        temp = getTemp geomId args selected
        tempValue = fst temp
        tempExp = snd temp
    in { model | tempValue <- tempValue
       , tempExp <- tempExp
       , selected <- selected
       , notAllowed <- notAllowed }
    
geomUpdate inputId model =
    let geomId = inputId
        old = model.selected ++ model.notAllowed
        len = length old
        selected = if | geomId == 0 -> take 1 old
                      | geomId == 2 || geomId == 3 -> take 2 old
                      | otherwise -> old
        notAllowed = drop (length selected) old
        args = map (\i -> head (drop i model.values)) selected
        temp = getTemp geomId args selected
        tempValue = fst temp
        tempExp = snd temp
    in { model | tempValue <- tempValue
               , tempExp <- tempExp
               , geomSelected <- geomId
               , selected <- selected
               , notAllowed <- notAllowed }

customUpdate inputId model =
    let len = length model.values
    in if | inputId == 0 {- + -} && len < 7 -> addUpdate model
          | inputId == 1 {- - -} && len > 0 -> minusUpdate model
          | otherwise -> model

-- Pre: assumes that length of model.values < 7
addUpdate model = 
    let values = case model.tempValue of
            V.Abyss -> model.values-- ++ [(head (drop (length model.values) C.vectors))]
            _ -> model.values ++ [model.tempValue]
        varId = (length values) - 1
        expressions = case model.tempValue of
            V.Abyss -> model.expressions
            _ -> model.expressions ++ [model.tempExp]
    in if | (length model.values) == (length values) -> model
          | otherwise ->  varUpdate varId { model | values <- values
                       , selected <- []
                       , notAllowed <- []
                       , expressions <- expressions 
                       , geomSelected <- 0 }

-- Pre: assumes that length of model.values > 0
minusUpdate model = 
    let varId = (length model.values) - 1 --get the varId to be removed
        expressions = filter (hasNot varId) (take varId model.expressions)
        values = take ((length model.values) - 1) model.values
        selected = filter (\var -> not (var == varId)) model.selected  --remove varId from selected
    in geomUpdate 0 { model | values <- values
                    , expressions <- expressions 
                    , selected <- selected
                    , notAllowed <- []}
                    
hasNot var lst = 
    case lst of 
    [] -> True 
    _ -> hasNotHelper var (tail lst)

hasNotHelper var lst =
    case lst of 
    [] -> True
    one::more -> if one == var 
                 then False 
                 else (hasNotHelper var more)
    
-- Pre: Assumes that values / selected represent same data (must have same length)
-- Post: Returns a pair: (value of evaluating a geometry, expression of eval)
getTemp geomId values selected =
    let len = length values 
    in if | len == 0 -> (V.Abyss, geomId::selected)
          | geomId == 0 -> (V.eval (V.Atom (head values)), 0::[last selected])
          | geomId == 1 -> (V.eval (V.Span values), 1::selected) 
          | geomId == 2 && len > 1 -> (V.eval (V.Project (head values) (head (tail values))), 2::selected)
          | geomId == 3 && len > 1 -> (V.eval (V.Reject (head values) (head (tail values))), 3::selected)
          | otherwise -> (V.Abyss, geomId::selected)


-- Render
render (w, h) model = 
    let varIndexes = [0 .. ((length model.values) - 1)]
        funIndexes = [0 .. ((length C.funs) - 1)]
        customIndexes = [0 .. ((length C.custom) - 1)]
        varButtons = buildButtons varInput' varIndexes (sort model.selected) (sort model.notAllowed) 
        geomButtons = buildButtons geomInput' funIndexes [model.geomSelected] []
        custButtons = buildButtons customInput' customIndexes [] []
        spacers = map (\i -> spacer 24 1) [1 .. (7 - (length varButtons))]  --pull out seven
        expression = getDef model.tempExp
        varDefinitions = defsFromExps (model.expressions++[model.tempExp])
        funDefinition = defFromFun model.geomSelected
    in container w h (topLeftAt (relative 0.2) (relative 0.2)) (flow down 
        [ (leftAligned (T.height 30 (monospace (toText "visual-algebra"))))
        , (leftAligned (T.height 10 (monospace (toText "v0.01 E-5 alpha"))))
        , spacer 10 10
        , (leftAligned (T.height 30 (monospace (toText expression))))
        , spacer 10 10
        , width 250 (flow right (varButtons ++ spacers ++ custButtons))
        , spacer 10 5  
        , flow down varDefinitions
        , spacer 10 10 
        , width 250 (flow right geomButtons)
        , spacer 10 5 
        , width 250 funDefinition
        --, width 250 (asText model)
     ])

-- Signals
--signals = lift2 (\a b c-> {geom=a, vars=[b], custom=}) geomInput.signal (foldp swap [0] vecInput.signal)
signals = merges [variable, geom, custom]
geom = lift (\v -> (v,"geom")) geomInput.signal 
variable = lift (\v -> (v,"variable")) varInput.signal 
custom = lift (\v -> (v,"custom")) customInput.signal 

-- Inputs
geomInput : Input Int
geomInput = input 0

varInput : Input Int
varInput = input 0
                
customInput : Input Int
customInput = input 0

geomInput' = {geomInput | name = "geom"}
varInput' = {varInput | name = "vector"}
customInput' = {customInput | name = "custom"}

-- Pre: assumes new is a number and old a number list
-- Post: returns a list, which swaps the state as to whether old contained new
toggleList : Int -> [Int] -> [Int]
toggleList new old = 
    case old of
     [] -> [new]
     one::more -> 
         if new == one
         then more
         else one::(toggleList new more)

--change variable names ugh sigh ugh
buildButtons inputType buttonIds chosen notAvail = 
    case buttonIds of
    [] -> []
    b::bs -> 
        let choice = if (length chosen) > 0 then head chosen else -1
            notA = if (length notAvail) > 0 then head notAvail else -1
            style  = if | choice == b -> 1 -- chosen button, dark style
                        | notA == b -> 2   -- chosen but not available, light style
                        | otherwise -> 0   -- not chosen, no styling
            chosenTail = if choice == b then tail chosen else chosen
            notATail = if notA == b then tail notAvail else notAvail
        in (getButton style inputType b)::(buildButtons inputType bs chosenTail notATail)
           
getButton buttonType inputType index =
    let name = if | inputType.name == "vector" -> head (drop index C.vars) 
                  | inputType.name == "geom" ->  head (drop index C.funs)
                  | inputType.name == "custom" -> head (drop index C.custom)
        styledText = if | buttonType == 1 -> line Under (toText name)
                        | otherwise -> toText name
        element = leftAligned (T.height 15 (monospace styledText))
        w = widthOf element
        h = heightOf element
        linked =  link "#" (container (w + 15) (h + 10) middle element)
        highlighted = if | buttonType == 1 ->  color (rgba 0 0 0 0.2) linked 
                         | buttonType == 2 -> color (rgba 0 0 0 0.1) linked
                         | otherwise -> linked
    in clickable inputType.handle index highlighted

defsFromExps : [[Int]] -> [Element]
defsFromExps exps =
    let len = length exps
        defs = getDefs (take 7 exps) [] 0
        elems =  map (\str -> leftAligned (T.height 15 (monospace (toText str)))) (take (len - 1) defs)
        temp = if len == 8 then []
               else [opacity 0.5 (leftAligned (T.height 15 (monospace (toText (last defs)))))]
    in elems ++ temp

getDefs : [[Int]] -> [String] -> Int -> [String]
getDefs listOfLists solutions index = 
    case listOfLists of
    [] -> solutions
    front::more -> 
        let def = getDef front
            var = head (drop index C.vars)
            result = var ++ " = " ++ def
        in getDefs more (solutions ++ [result]) (index + 1)

-- Pre: length exp > 0
getDef : [Int] -> String
getDef exp =
    let funId = head exp
        otherVars = map (\i -> head (drop i C.vars)) (tail exp)
        len = length otherVars
        funName = 
            if | funId == 0 -> "atom"
               | funId == 1 -> "span"
               | funId == 2 -> "projection"
               | funId == 3 -> "rejection"
        prep = " of "
        inBetw = 
            if | (funId == 0 || funId == 1) && len > 0 -> concat (intersperse ", " otherVars)
               | (funId == 0 || funId == 1) -> "_"
               | (funId == 2 || funId == 3) && len > 1 -> head otherVars ++ " onto " ++ (head (tail otherVars))
               | (funId == 2 || funId == 3) && len == 1 -> head otherVars ++ " onto _" 
               | (funId == 2 || funId == 3) && len == 0 -> "_ onto _" 
    in "the " ++ funName ++ prep ++ inBetw 

defFromFun funId =
    let def = (head (drop funId C.defs))
    in leftAligned (T.height 15 (monospace (toText def)))
