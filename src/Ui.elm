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
    
    FIX TEMPORARY

    bring constants to module? 
    extract similar code in update methods (eval geom part) (eval geom part)
    pull out all 7s in render, add to Constants
    getButton skips buttonStates of 3 kinda hacky
    
    grammar:
    exprs : expressions
    temp(Value|Exp) : represents the evaluated geometry before being stored as a value
    *Update : helper methods which handle * type of input (variable button/plus button)
    buttonAction : hover/click ->  0/1
    buttonType : fun/var/meta -> 0/1/2
    buttonState : rest/hover/clicked/hid : 0/1/2/3 

    assumptions:

-}

main = render <~ Window.dimensions ~ (foldp update model signals)

-- Model
model = { exprs = C.expressions -- [[functionID, varID,..]], list of var expr
        , values = C.values -- list of vectors behind all expressions
        , temp = [] -- [[functionID, varID , ...]]
        , funs = A.repeat (length C.funs) 0
        , vars = A.repeat (length C.vars) 0 --  transparent state
        , meta = A.repeat (length C.meta) 2 -- transparent state
        , index = 0 -- index of expr
        , expr = E.Empty 
        }

data Action = Click | Hover | None
data State = Available | Hidden
data Button = Fun Int State | Var Int State | Meta Int State
-- Update
-- all button interactions (clicks/hovers) constitute the entire events sent to UI
-- update redirects model updates based on type of update (fun/variable/meta)
update (action, button) model =
    case button of
    Fun  _ Hidden -> model
    Var  _ Hidden -> model
    Meta _ Hidden -> model
    _ -> 
    (case action of 
    Hover ->
        case button of 
        Var  index _ -> { model | vars <- A.set index 1 model.vars }
        Fun  index _ -> { model | funs <- A.set index 1 model.funs }
        Meta index _ -> { model | meta <- A.set index 1 model.meta }
    Click -> 
        case button of
        Var  _ _ -> varUpdate  button model
        Fun  _ _ -> funUpdate  button model
        Meta _ _ -> metaUpdate button model
    None -> 
        case button of 
        Var  index _ -> { model | vars <- A.set index 0 model.vars }
        Fun  index _ -> { model | funs <- A.set index 0 model.funs }
        Meta index _ -> { model | meta <- A.set index 0 model.meta })



-- Pre: assumes buttonId refers to the id of a variable button
-- Post: updates model based on the button clicked
varUpdate button model =
    let index = model.index -- index of expression to replace value
        expr = case button of 
                Fun buttonId _ -> E.replace index model.expr (E.Leaf buttonId)
                Var buttonId _ -> E.replace index model.expr (E.Leaf buttonId)
                Meta buttonId _ -> E.replace index model.expr (E.Leaf buttonId)
    in { model | expr <- expr 
       , index <- index + 1 }

-- Pre: assumes buttonId refers to the id of a variable button
-- Post: updates model based on the button clicked
funUpdate button model =
    let index = model.index -- index of expression to replace value
        funId = case button of
                Fun buttonId _ -> buttonId
                Var buttonId _ -> buttonId
                Meta buttonId _ -> buttonId
        def = if | funId == 4 || funId == 5 -> E.Node funId [E.Empty]
                 | otherwise -> E.Node funId [E.Empty, E.Empty]
        expr = E.replaceNode index model.expr funId def -- replaces directly if possible
    in { model | expr <- expr 
       , index <- index + 1 }

metaUpdate button model = model

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
        , (leftAligned (T.height 30 (monospace (toText expression))))
        , spacer 10 10
        , width 250 (flow right varButtons)
        , height 90 (width 250 (flow right funButtons))
        , height 90 (width 250 (flow right metaButtons))
   --   , width 250 (flow down varDefinitions)
   --   , width 250 funDefinition
        , width 250 (asText model)
     ])

-- Signals
signals : Signal (Action, Button)
signals = inputs.signal

inputs : Input (Action, Button)
inputs = input (None, Var 0 Available)

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
        sigState = if buttonState == 0 || buttonState == 1 then Available else Hidden -- 3 is considered above
        sigButton = if | buttonType == 0 -> Fun  index sigState  
                       | buttonType == 1 -> Var  index sigState  
                       | buttonType == 2 -> Meta index sigState  
        styled = if | buttonState == 0 -> linked
                    | buttonState == 1 -> (color (rgba 0 0 0 0.1) linked)
                    | otherwise -> opacity 0.5 linked
        b = clickable inputs.handle (Click, sigButton) styled
    in hoverable inputs.handle (\bool -> if bool then (Hover, sigButton) else (None, sigButton)) b

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
