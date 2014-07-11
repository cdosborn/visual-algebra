module Ui where

import Window
import Graphics.Input (..)
import Array as A
import Text as T

import Vector as V
import Constants as C
import Expr as E


{-
    TODO:
    
    extract similar code in update methods (eval geom part) (eval geom part)
    pull out all 7s in render, add to Constants
    getButton skips buttonStates of 3 kinda hacky
    historyLimit cannot be 0, undo/redo get messed up (undo needs to be fixed should be simple)
    
    grammar:

    assumptions:
-}

main = render <~ Window.dimensions ~ state
state = foldp update C.model signals


-- Model
--defined in Constants as C.model


-- Update
-- all button interactions (clicks/hovers) constitute the entire events sent to UI
-- update redirects model updates based on type of update (fun/variable/meta)
update : (C.Action, C.Button) -> C.Model -> C.Model
update (action, button) model =
    case button of
    C.Fun  _ (C.Hidden) -> model
    C.Var  _ (C.Hidden) -> model
    C.Meta _ (C.Hidden) -> model
    _ -> 
    (case action of 
    C.Hover ->
        case button of 
        C.Var  index _ -> { model | vars <- A.set index 1 model.vars }
        C.Fun  index _ -> { model | funs <- A.set index 1 model.funs }
        C.Meta index _ -> { model | meta <- A.set index 1 model.meta }
    C.Click -> 
        let m = { model | history <- button::model.history 
                , meta <- A.set 1 0 model.meta}
        in 
            case button of
            C.Var  index _ -> varUpdate  index m
            C.Fun  index _ -> funUpdate  index m
            C.Meta index _ -> metaUpdate index m
    C.None -> 
        case button of 
        C.Var  index _ -> { model | vars <- A.set index 0 model.vars }
        C.Fun  index _ -> { model | funs <- A.set index 0 model.funs }
        C.Meta index _ -> { model | meta <- A.set index 0 model.meta })



-- Pre: assumes buttonId refers to the id of a variable button
-- Post: updates model based on the button clicked
varUpdate varId model =
    let index = model.index -- index of expression to replace value
        expr =  E.replace index model.expr (E.Leaf varId)
    in { model | expr <- expr 
       , index <- index + 1 }

-- Pre: assumes buttonId refers to the id of a variable button
-- Post: updates model based on the button clicked
funUpdate funId model =
    let index = model.index -- index of expression to replace value
        def = if | funId == 4 || funId == 5 -> E.Node funId [E.Empty]
                 | otherwise -> E.Node funId [E.Empty, E.Empty]
        expr = E.replaceNode index model.expr funId def -- replaces directly if possible
    in { model | expr <- expr 
       , index <- index + 1 }

metaUpdate metaId model =
  if | metaId == 1 || metaId == 2 -> onUndo model
     | otherwise -> model
 
-- Pre: assumes model.history contains at least [undoButton, aChange]
-- Post: resets the model to previous state
onUndo model =
    let limit = historyLength model.history
        dif = limit - C.historyLimit
        (history, base') = 
            if dif > 0 -- limit is larger
            then (take (C.historyLimit + 1) model.history, getChanges (drop (C.historyLimit + 1) model.history))
            else (model.history, [])
        meta' =  if showUndo history then model.meta else A.set 1 2 model.meta 
        meta'' = if showRedo history then A.set 2 0 meta' else A.set 2 2 meta'
        base = base' ++ model.base
        changes = getChanges (history ++ base) -- reduces history into correct statee
        clickList = repeat (length (changes)) C.Click
        args = zip clickList changes
        m = foldr (\arg modl -> update arg modl) C.model args
    in { m | history <- history 
       , base <- base 
       , meta <- meta'' 
       , basis <- model.basis
       , target <- model.target
       , units <- model.units}
    --_ -> fail otherwise history should always have 2 on undo

-- Post: returns length of history, minus initial consecutive undos/redos
historyLength : [C.Button] -> Int
historyLength history =
    case history of
    (C.Meta 1 _)::more -> historyLength more
    (C.Meta 2 _)::more -> historyLength more    
    _ -> length history

showUndo : [C.Button] -> Bool
showUndo history = showUndoAux history 0
showUndoAux history num =
    case history of
    (C.Meta 1 _)::more-> showUndoAux more (num + 1) 
    (C.Meta 2 _)::more->  showUndoAux more (num - 1) 
    _ -> num < C.historyLimit && num < (length history)


showRedo : [C.Button] -> Bool
showRedo history = showRedoAux history 0
showRedoAux history index =
    case history of
    (C.Meta 1 _)::more-> index == 0 || (showRedoAux more (index - 1))
    (C.Meta 2 _)::more-> showRedoAux more (index + 1)
    _ -> False

-- Post: irreversibly reduces the history by removing oldest undo/redo
--       expression to what is currently intended, includes count of undos removed                  
compressHistory : [C.Button] -> [C.Button]
compressHistory history = compressAux history [] 0 0
compressAux history solution undos redos =
    if history == [] then history else 
    let end = last history 
        rest = take (length history - 1) history
    in case end of
        C.Meta 1 _ -> compressAux rest solution (undos + 1) redos 
        C.Meta 2 _ -> compressAux rest solution undos (redos + 1)
        _ -> -- fun or var button
            if undos == 0 -- havn't reached undo or redo yet
            then compressAux rest (end::solution) 0 0
            else history ++ (drop (undos - redos) solution)

---- Pre: assumes model.future contains at least [futButton]
--onRedo model =
--    case model.history of 
--    redo::changes -> --first argument alwayws redo button
--        let history = changes  --hist doesn't include instruction to undo
--            arg = (C.Click, head model.future)
--            m = update arg { model | history <- history }
--        in { m | future <- tail model.future 
--           , history <- model.history }
--    --_ -> model -- throw error

-- Pre: in history of buttons, redos always precede undos
-- Post: filters every button pressed, reducing a list of undos,redos,etc
--       into actual history
getChanges : [C.Button] -> [C.Button]
getChanges history = getChangesAux history [] 0
getChangesAux history changes undo =
    case history of
    [] -> changes
    (C.Meta 1 _)::more -> -- undo
        getChangesAux more changes (undo + 1)
    _ -> 
        if undo > 0 
        then getChangesAux (drop undo history) changes 0
        else  -- if no undos then accumulate redos/funs/vars
            case history of 
            (C.Meta 2 _)::more -> getChangesAux more changes (undo - 1)
            other::more -> getChangesAux more (changes++[other]) 0

-- Pre: assumes that length of model.values < 7
--addUpdate model = 
--    let values = case model.tempValue of
--            V.Abyss -> model.values-- ++ [(head (drop (length model.values) C.values))]
--            _ -> model.values ++ [model.tempValue]
--        varId = (length values) - 1
--        expressions = case model.tempValue of
--            V.Abyss -> model.expressions
--            _ -> model.expressions ++ [model.tempExp]
--    in if | (length model.values) == (length values) -> model
--          | otherwise ->  varUpdate varId { model | values <- values
--                       , varQueue <- []
--                       , notAllowed <- []
--                       , expressions <- expressions 
--                       , funQueue <- 0 }
--
---- Pre: assumes that length of model.values > 0
--minusUpdate model = 
--    let varId = (length model.values) - 1 --get the varId to be removed
--        expressions = filter (hasNot varId) (take varId model.expressions)
--        values = take ((length model.values) - 1) model.values
--        varQueue = filter (\var -> not (var == varId)) model.varQueue  --remove varId from varQueue
--    in funUpdate 0 { model | values <- values
--                    , expressions <- expressions 
--                    , varQueue <- varQueue
--                    , notAllowed <- []}
                    
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
    
-- Pre: Assumes that values / varQueue represent same data (must have same length)
-- Post: Returns a pair: (value of evaluating a geometry, expression of eval)
getTemp funId values varQueue =
    let len = length values 
    in if | len == 0 -> (V.Abyss, funId::varQueue)
          | funId == 0 -> (V.eval (V.Atom (head values)), 0::[last varQueue])
          | funId == 1 -> (V.eval (V.Span values), 1::varQueue) 
          | funId == 2 && len > 1 -> (V.eval (V.Project (head values) (head (tail values))), 2::varQueue)
          | funId == 3 && len > 1 -> (V.eval (V.Reject (head values) (head (tail values))), 3::varQueue)
          | otherwise -> (V.Abyss, funId::varQueue)

-- Render
render (w, h) model = 
    let funButtons  = (buildButtons 0 model.funs)
        varButtons  = (buildButtons 1 model.vars)
        metaButtons = (buildButtons 2 model.meta)
        spacers = map (\i -> spacer 24 1) [1 .. (7 - (length varButtons))]  --pull out seven
        expression = E.toString C.funs C.vars model.expr
  --    varDefinitions = defsFromExps (model.expressions++[model.tempExp])
  --    funDefinition = defFromFun model.funQueue
    in container w h (topLeftAt (relative 0.2) (relative 0.2)) (flow down 
        [ (leftAligned (T.height 30 (monospace (toText "visual-algebra"))))
        , (leftAligned (T.height 10 (monospace (toText "v0.01 E-5 alpha"))))
        , spacer 10 10
        , width 700 (leftAligned (T.height 30 (monospace (toText expression))))
        , spacer 10 10
        , width 250 (flow right varButtons)
        , height 90 (width 250 (flow right funButtons))
        , height 90 (width 250 (flow right metaButtons))
   --   , width 250 (flow down varDefinitions)
   --   , width 250 funDefinition
     ])

-- Signals
type Inputs = (C.Action, C.Button)
signals : Signal Inputs
signals = inputs.signal

inputs : Input Inputs
inputs = input (C.None, C.Var 0 C.Available)

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

-- maybe return an array instead then list
buildButtons : Int -> A.Array Int -> [Element]
buildButtons buttonType buttonArr =
    A.toList (A.indexedMap (\i state -> getButton buttonType state i) buttonArr)
           
--implement hoverable?
getButton : Int -> Int -> Int -> Element
getButton buttonType buttonState index =
    if buttonState == 3 then empty else  -- KINDA HACKY, these buttons not drawn
    let name = if | buttonType == 0 -> head (drop index C.funs) -- function
                  | buttonType == 1 -> head (drop index C.vars) -- var
                  | buttonType == 2 -> head (drop index C.meta) -- meta 
        element = leftAligned (T.height 15 (monospace (toText name)))
        w = widthOf element
        h = heightOf element
        linked =  link "#" (container (w + 15) (h + 10) middle element)
        sigState = if buttonState == 0 || buttonState == 1 then C.Available else C.Hidden -- 3 is considered above
        sigButton = if | buttonType == 0 -> C.Fun  index sigState  
                       | buttonType == 1 -> C.Var  index sigState  
                       | buttonType == 2 -> C.Meta index sigState  
        styled = if | buttonState == 0 -> linked
                    | buttonState == 1 -> (color (rgba 0 0 0 0.1) linked)
                    | otherwise -> opacity 0.5 linked
        b = clickable inputs.handle (C.Click, sigButton) styled
    in hoverable inputs.handle (\bool -> if bool then (C.Hover, sigButton) else (C.None, sigButton)) b

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
        funName = head (drop funId C.funs) 
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
