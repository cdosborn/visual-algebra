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
        case button of
            C.Var  index _ -> varUpdate  index model
            C.Fun  index _ -> funUpdate  index model
            C.Meta index _ -> metaUpdate index model
    C.None -> 
        case button of 
        C.Var  index _ -> { model | vars <- A.set index 0 model.vars }
        C.Fun  index _ -> { model | funs <- A.set index 0 model.funs }
        C.Meta index _ -> { model | meta <- A.set index 0 model.meta })


-- Pre: assumes buttonId refers to the id of a variable button
-- Post: updates model based on the button clicked
varUpdate varId model =
    let index = model.index -- index of expression to replace value
        expr = E.replace index model.expr (E.Ref varId)
        value = V.eval (exprToSpace expr model.exprs model.values)
        history = (C.Var varId C.Available)::model.history 
        indexAtEnd = atEnd expr index
    in { model | expr <- expr 
       , value <- value
       , index <- index + 1 
       , vars <- if indexAtEnd then setN (length model.values) 2 model.vars else model.vars 
       , funs <- if indexAtEnd then setN (A.length model.funs) 2 model.funs else model.funs
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
        history = (C.Fun funId C.Available)::model.history 
    in { model | expr <- expr 
       , index <- index + 1
       , meta <- getMeta model.meta history expr model.exprs model.value
       , history <- history }
 
metaUpdate metaId model = 
    let m = { model | history <- (C.Meta metaId C.Available)::model.history } in
    if | metaId == 0 -> onSave m
       | metaId == 1 || metaId == 2 -> onUndo m
       | metaId == 3 -> onClear m
       | otherwise -> model

onSave model =
    let exprs = model.exprs ++ [model.expr]
        values = model.values ++ [model.value]
    in { model | expr <- E.Empty
       , exprs <- exprs
       , value <- V.Abyss
       , values <- values
       , index <- 0
       , vars <- setN (length values) 0 model.vars
       , funs <- setN (A.length model.funs) 0 model.funs
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
        changes = getChanges (history ++ base) -- reduces history into correct statee
        clickList = repeat (length (changes)) C.Click
        args = zip clickList changes
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
    , vars <- setN (length model.values) 0 model.vars
    , funs <- setN (A.length model.funs) 0 model.funs
    , meta <- getMeta model.meta model.history E.Empty model.exprs V.Abyss}

--onQuestion model =
--    { model | query <- True
--    , info <- "select any button to learn about its function" }

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

showSave : [E.Expr] -> V.Space -> Bool
showSave exprs value = 
    (length exprs) < 7 && ((V.mEval 1.0 value) /= V.Abyss)

showClear : E.Expr -> Bool
showClear expr =
    (not (expr == E.Empty))

getMeta meta history expr exprs value =
    let meta'    = if showUndo history     then A.set 1 0 meta    else A.set 1 2 meta 
        meta''   = if showRedo history     then A.set 2 0 meta'   else A.set 2 2 meta'
        meta'''  = if showSave exprs value then A.set 0 0 meta''  else A.set 0 2 meta''
        meta'''' = if showClear expr       then A.set 3 0 meta''' else A.set 3 2 meta'''
    in meta''''

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

---- Pre: Assumes that values / varQueue represent same data (must have same length)
---- Post: Returns a pair: (value of evaluating a geometry, expression of eval)
--getTemp funId values varQueue =
--    let len = length values 
--    in if | len == 0 -> (V.Abyss, funId::varQueue)
--          | funId == 0 -> (V.eval (head values), 0::[last varQueue])
--          | funId == 1 -> (V.eval (V.Span values), 1::varQueue) 
--          | funId == 2 && len > 1 -> (V.eval (V.Project (head values) (head (tail values))), 2::varQueue)
--          | funId == 3 && len > 1 -> (V.eval (V.Reject (head values) (head (tail values))), 3::varQueue)
--          | otherwise -> (V.Abyss, funId::varQueue)

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
        , height 69 (width 250 (flow right funButtons))
        , height 46 (width 250 (flow right metaButtons))
        --, (spacer 5 1) `beside` (width 250 (leftAligned (T.height 15 (monospace (toText model.info)))))
--      , width 500 (asText model)
--      , width 500 (asText model.expr)
--      , width 500 (asText model.exprs)
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
        element =
            if False--buttonType == 1
            then leftAligned (T.height 15 (monospace (T.color (head (drop index C.colors)) (toText name))))
            else leftAligned (T.height 15 (monospace (toText name)))
        w = widthOf element
        h = heightOf element
        linked =  link "#" (container (w + 10) (h + 5) middle element)
        sigState = if buttonState == 0 || buttonState == 1 then C.Available else C.Hidden -- 3 is considered above
        sigButton = if | buttonType == 0 -> C.Fun  index sigState  
                       | buttonType == 1 -> C.Var  index sigState  
                       | buttonType == 2 -> C.Meta index sigState  
        styled = if | buttonState == 0 -> linked
                    | buttonState == 1 -> (color (rgba 0 0 0 0.1) linked)
                    | otherwise -> opacity 0.5 linked
        b = clickable inputs.handle (C.Click, sigButton) styled 
    --in hoverable inputs.handle (\bool -> if bool then (C.Hover, sigButton) else (C.None, sigButton)) b
    in b

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
--getInfo : C.Button -> [E.Expr] -> String
--getInfo b exprs = 
--    case b of
--    C.Fun index _ -> head (drop index C.defs)
--    C.Meta index _ -> head (drop (index + (length C.funs)) C.defs) 
--    C.Var index _ -> 
--        let name = head (drop index C.vars) 
--        in name ++ " = " ++ (E.toString C.funs C.vars (head (drop index exprs)))

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

