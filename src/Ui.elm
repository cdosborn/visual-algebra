module Ui where

import Window
import Graphics.Input (..)
import Array as A
import Text as T

import Vector as V
import Constants as C
import Expr as E
import Debug (log)


{-
    TODO:
    
    if a button is transparent, dont create a button -- doesn't work, strange ui errors
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
update : C.Button -> C.Model -> C.Model
update btn model =
    if model.inQuery
    then queryUpdate btn model
    else 
    case btn of
    C.Var  index -> varUpdate  index model
    C.Fun  index -> funUpdate  index model
    C.Meta index -> metaUpdate index model
    C.None -> model


-- Pre: assumes buttonId refers to the id of a variable button
-- Post: updates model based on the button clicked
varUpdate varId model =
    let index = model.index -- index of expression to replace value
        expr = E.replace index model.expr (E.Ref varId)
        value = exprToSpace expr model.exprs model.values
        history = (C.Var varId)::model.history 
    in { model | expr <- expr 
       , value <- value
       , index <- if atEnd expr index then index else index + 1
--     , vars <- setN (length model.values) 0 model.vars
--     , funs <- if indexAtEnd then setN (A.length model.funs) 2 model.funs else model.funs
       , meta <- getMeta model.meta history expr model.exprs value
       , history <- history }

-- Post: returns true if the current index points to end
--       of expr
atEnd expr index = (E.count expr) == (index + 1)
setN n x arr =
    if n == 0 then arr
    else setN (n - 1) x (A.set (n - 1) x arr)
    

-- Pre: assumes buttonId refers to the id of a variable button
-- Post: updates model based on the button clicked
funUpdate funId model =
    let index = model.index -- index of expression to replace value
        funExpr = 
            if funId > 3 && funId < 7 
            then E.Unary funId E.Empty 
            else E.Duo funId E.Empty E.Empty
        expr = E.replace index model.expr funExpr -- replaces directly if possible
        history = (C.Fun funId)::model.history 
    in { model | expr <- expr 
       , index <- if atEnd expr index then index else index + 1
       , meta <- getMeta model.meta history expr model.exprs model.value
       , history <- history }
 
metaUpdate metaId model = 
    let m = { model | history <- (C.Meta metaId)::model.history } in
    if | metaId == 0 -> onSave m
       | metaId == 1 || metaId == 2 -> onUndo m
       | metaId == 3 -> onClear m
       | metaId == 4 -> onQuestion model 
       | metaId == 5 -> C.model
       | otherwise -> model

queryUpdate btn model = 
    case btn of
    C.Meta 4 -> { model | inQuery <- False, query <- C.None, info <- ""
                , meta <- getMeta (A.set 4 0 model.meta) model.history model.expr model.exprs model.value }
    _ -> { model | query <- btn, info <- getInfo btn model.exprs }


onSave model =
    let exprs = model.exprs ++ [model.expr]
        values = model.values ++ [model.value]
    in { model | expr <- E.Empty
       , exprs <- exprs
       , value <- V.Abyss
       , values <- values
       , vars <- setN (length values) 0 model.vars
       , index <- 0
       , meta <- getMeta model.meta model.history E.Empty exprs V.Abyss}


 
-- Pre: assumes model.history contains at least [undoButton, aChange]
-- Post: resets the model to previous state
onUndo model =
    let limit = historyLength model.history
        dif = limit - C.historyLimit
        (history, base') = 
            if dif > 0 -- limit is larger
            then (take (C.historyLimit + 1) model.history, getChanges (drop (C.historyLimit + 1) model.history))
            else (model.history, [])
        base = base' ++ model.base
        args = getChanges (history ++ base) -- reduces history into correct statee
        m = foldr (\arg modl -> update arg modl) C.model args
    in { m | history <- history 
       , base <- base 
       , meta <- getMeta m.meta history m.expr m.exprs m.value
       , basis <- model.basis
       , units <- model.units}
    --_ -> fail otherwise history should always have 2 on undo

onClear model =
    { model | expr <- E.Empty
    , value <- V.Abyss
    , index <- 0 
    , meta <- getMeta model.meta model.history E.Empty model.exprs V.Abyss}

onQuestion model =
    { model | inQuery <- True
    , meta <- A.set 4 1 <| setN (A.length model.meta) 0 model.meta
    }

-- Post: returns length of history, minus initial consecutive undos/redos
historyLength : [C.Button] -> Int
historyLength history =
    case history of
    (C.Meta 1)::more -> historyLength more
    (C.Meta 2)::more -> historyLength more    
    _ -> length history

showUndo : [C.Button] -> Bool
showUndo history = showUndoAux history 0
showUndoAux history num =
    case history of
    (C.Meta 1)::more -> showUndoAux more (num + 1) 
    (C.Meta 2)::more ->  showUndoAux more (num - 1) 
    _ -> num < C.historyLimit && num < (length history)


showRedo : [C.Button] -> Bool
showRedo history = showRedoAux history 0
showRedoAux history index =
    case history of
    (C.Meta 1)::more-> index == 0 || (showRedoAux more (index - 1))
    (C.Meta 2)::more-> showRedoAux more (index + 1)
    _ -> False

showSave : [E.Expr] -> V.Space -> Bool
showSave exprs value = 
    (length exprs) < 7 && ((V.eval 1.0 value) /= V.Abyss)

showClear : E.Expr -> Bool
showClear expr =
    (not (expr == E.Empty))

showReset : [C.Button] -> Bool
showReset history =
    history /= []

getMeta meta history expr exprs value =
    let meta'     = if showUndo history     then A.set 1 0 meta     else A.set 1 2 meta 
        meta''    = if showRedo history     then A.set 2 0 meta'    else A.set 2 2 meta'
        meta'''   = if showSave exprs value then A.set 0 0 meta''   else A.set 0 2 meta''
        meta''''  = if showClear expr       then A.set 3 0 meta'''  else A.set 3 2 meta'''
        meta''''' = if showReset history    then A.set 5 0 meta'''' else A.set 5 2 meta''''
    in meta'''''

-- Post: irreversibly reduces the history by removing oldest undo/redo
--       expression to what is currently intended, includes count of undos removed                  
compressHistory : [C.Button] -> [C.Button]
compressHistory history = compressAux history [] 0 0
compressAux history solution undos redos =
    if history == [] then history else 
    let end = last history 
        rest = take (length history - 1) history
    in case end of
        C.Meta 1 -> compressAux rest solution (undos + 1) redos 
        C.Meta 2 -> compressAux rest solution undos (redos + 1)
        _ -> -- fun or var button
            if undos == 0 -- havn't reached undo or redo yet
            then compressAux rest (end::solution) 0 0
            else history ++ (drop (undos - redos) solution)

-- Pre: in history of buttons, redos always precede undos
-- Post: filters every button pressed, reducing a list of undos,redos,etc
--       into actual history
getChanges : [C.Button] -> [C.Button]
getChanges history = getChangesAux history [] 0
getChangesAux history changes undo =
    case history of
    [] -> changes
    (C.Meta 1)::more -> -- undo
        getChangesAux more changes (undo + 1)
    _ -> 
        if undo > 0 
        then getChangesAux (drop undo history) changes 0
        else  -- if no undos then accumulate redos/funs/vars
            case history of 
            (C.Meta 2)::more -> getChangesAux more changes (undo - 1)
            other::more -> getChangesAux more (changes++[other]) 0

-- Render
render (w, h) model = 
    let funButtons  = buildButtons 0 model.funs
        varButtons  = buildButtons 1 model.vars
        metaButtons = buildButtons 2 model.meta
        spacers = map (\i -> spacer 24 1) [1 .. (7 - (length varButtons))]  --pull out seven
        expression = 
            if model.inQuery 
            then queryToString model.query
            else E.toString C.funs C.vars model.expr
--      varDefinitions = defsFromExps (model.expressions++[model.tempExp])
--      funDefinition = defFromFun model.funQueue
    in container w h (topLeftAt (relative 0.2) (relative 0.2)) (flow down 
        [ (leftAligned (T.height 30 (monospace (toText "visual-algebra"))))
        , (leftAligned (T.height 10 (monospace (toText "v. 1.0e-5 alpha alpha ..."))))
        , spacer 10 10
        , width 700 (leftAligned (T.height 30 (monospace (toText expression))))
        , spacer 10 10
        , width 200 (flow right varButtons)
        , height 69 (width 200 (flow right funButtons))
        , spacer 10 20
        , height 46 (width 200 (flow right metaButtons))
        , spacer 200 23
        , (spacer 5 1) `beside` (width 500 (leftAligned (T.height 15 (monospace (toText model.info)))))
--      , width 500 (asText model.query)
--      , width 500 (asText model.expr)
--      , width 500 (asText model.exprs)
     ])

queryToString query =
    case query of
    C.None -> "? _ "
    C.Fun  i -> "? " ++ (head (drop i C.funs))
    C.Var  i -> "? " ++ (head (drop i C.vars))
    C.Meta i -> "? " ++ (head (drop i C.meta))

-- Signals
type Inputs = C.Button
signals : Signal Inputs
signals = inputs.signal

inputs : Input Inputs
inputs = input (C.None)

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
           
-- ought to be reworked with case Button of
getButton : Int -> Int -> Int -> Element
getButton buttonType buttonState index =
    if buttonState == 3 then empty else  -- KINDA HACKY, these buttons not drawn
    let name = if | buttonType == 0 -> head (drop index C.funs) -- function
                  | buttonType == 1 -> head (drop index C.vars) -- var
                  | buttonType == 2 -> head (drop index C.meta) -- meta 
        element =
            if False--buttonType == 1
            then leftAligned (T.height 15 (monospace (T.color (head (drop index C.colors)) (toText name))))
            else leftAligned (T.height 15 (monospace (toText name)))
        w = widthOf element
        h = heightOf element
        contained = container (w + 10) (h + 5) middle element
        linked = link "#" contained
        styled = if | buttonState == 0 -> linked
                    | buttonState == 1 -> color (rgba 0 0 0 0.3) linked
                    | otherwise -> opacity 0.5 linked
        btn = if | buttonState == 2 -> C.None
                 | buttonType == 0 -> C.Fun  index
                 | buttonType == 1 -> C.Var  index
                 | buttonType == 2 -> C.Meta index

    in clickable inputs.handle btn styled 
------- if buttonState == 2
------- then styled
------- else clickable inputs.handle btn styled 
        

--defsFromExps : [[Int]] -> [Element]
--defsFromExps exps =
--    let len = length exps
--        defs = getDefs (take 7 exps) [] 0
--        elems =  map (\str -> leftAligned (T.height 15 (monospace (toText str)))) (take (len - 1) defs)
--        temp = if len == 8 then []
--               else [opacity 0.5 (leftAligned (T.height 15 (monospace (toText (last defs)))))]
--    in elems ++ temp
--
--getDefs : [[Int]] -> [String] -> Int -> [String]
--getDefs listOfLists solutions index = 
--    case listOfLists of
--    [] -> solutions
--    front::more -> 
--        let def = getDef front
--            var = head (drop index C.vars)
--            result = var ++ " = " ++ def
--        in getDefs more (solutions ++ [result]) (index + 1)
--
-- Pre: length exp > 0
getInfo : C.Button -> [E.Expr] -> String
getInfo b exprs = 
    case b of
    C.Fun  index -> head (drop index C.funDefs)
    C.Meta index -> head (drop index C.metaDefs)
    C.Var  index -> 
        let name = head (drop index C.vars) 
        in 
            name ++ " = " ++ (E.toString C.funs C.vars (head (drop index exprs)))

exprToSpace expr exprs values =
    case expr of
    E.Empty -> V.Abyss
    E.Val valId -> head (drop valId values)
    E.Ref varId -> exprToSpace (head (drop varId exprs)) exprs values
    E.Unary funId e -> let s = exprToSpace e exprs values in
        if | funId == 4 -> V.Unit s
           | funId == 5 -> V.Negate s
           | funId == 6 -> V.Scale s
           | funId == 8 -> V.Trace s
    E.Duo funId a b -> 
        let s1 = exprToSpace a exprs values
            s2 = exprToSpace b exprs values
        in if | funId == 0 -> V.Add s1 s2
              | funId == 1 -> V.Subtract s1 s2
              | funId == 2 -> V.Project s1 s2
              | funId == 3 -> V.Reject s1 s2
              | funId == 7 -> V.Rotate s1 s2

