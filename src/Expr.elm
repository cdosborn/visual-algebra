module Expr where

{-
    TODO:
-}

-- Expr is used to model expressions composed of sub-expressions or values
--      a Leaf is a value reference, a Unary or Duo are fun references
--      that contain subexpressions
data Expr = Empty | Val Int | Ref Int | Unary Int Expr | Duo Int Expr Expr
 
toString : [String] -> [String] -> Expr -> String
toString funIndex varIndex expr = 
    case expr of
    Empty -> "_"
    Val i -> "vector"
    Ref i -> head (drop i varIndex)
    Unary i e -> 
        let funName = head (drop i funIndex)
            child = (toString funIndex varIndex) e
        in funName ++ "(" ++ child ++ ")"
    Duo i a b ->
        let funName = head (drop i funIndex)
            children = (a |> toString funIndex varIndex)
                       ++ " " ++
                       (b |> toString funIndex varIndex)
        in funName ++ "(" ++ children ++ ")"
-- fail if trying to make Val int into a string

getDependencies expr = getDependenciesHelper expr []
getDependenciesHelper expr solutions =
    case expr of
    Empty -> solutions
    Val i -> solutions
    Ref i -> i::solutions
    Unary i e -> getDependenciesHelper e solutions
    Duo i a b -> getDependenciesHelper b (getDependenciesHelper a solutions)


-- ensures that the inserted node in replace,
-- uses the previous exprs arguments.
setExpr : Expr -> Expr -> Expr
setExpr tree expr  =
    case tree of
    Unary _ e -> 
        case expr of 
        Unary i _ -> Unary i e
        _ -> expr
    Duo _ a b -> 
        case expr of 
        Duo i _ _ -> Duo i a b
        _ -> expr
    _ -> expr

replace : Int -> Expr -> Expr -> Expr
replace pos tree expr =
    if pos == 0 
    then setExpr tree expr
    else case tree of -- search tree until pos == 0
        Val _ -> tree -- nowhere to go, invalid pos
        Ref _ -> tree -- nowhere to go, invalid pos
        Unary funId e -> Unary funId (replace (pos - 1) e expr)
        Duo funId a b -> let depthOfA = count a in
            if depthOfA >= pos 
            then Duo funId (replace (pos - 1) a expr) b
            else Duo funId a (replace (pos - depthOfA - 1) b expr)
        _ -> tree

-- counts the number of branches and leaves in a tree
count : Expr -> Int
count tree = 
    case tree of
    Empty -> 1
    Val _ -> 1
    Ref _ -> 1
    Unary _ e -> 1 + (count e)
    Duo _ a b -> 1 + (count a) + (count b)
                                                                                
---- returns Maybe (index, value) for a value in a list which satisfies predicate
--first : ( a -> Bool) -> [a] -> Maybe (Int, a)
--first check list = firstHelper check list 0
--
--firstHelper check list index =
--    case list of
--    [] -> Nothing
--    value::rest -> if (check value) then Just (index, value) else firstHelper check rest (index + 1)
--
---- folds over transformed values returning (index, value, accum) when the fold satisfies the predicate.
----      init is the initial value of the fold
----
----      Discussion:
----      The complexity comes from the desire to check the predicate of expensive transformations w/o
----      transforming all values. The unit function can be passed to change to a foldUntil method or 
----      a fold method which always returns the second value instead of the combination would result
----      in a mapUntil method.
--mapAndFoldUntil f fold init check list =
--    case list of
--    [] -> Nothing
--    front::rest -> let value = f front in
--        if check value 
--        then Just (0, front, init) 
--        else mapAndFoldUntilHelper f fold value check rest 1
--
--mapAndFoldUntilHelper f fold prev check list index =
--    case list of
--    [] -> Nothing
--    next::rest -> let combined = (fold prev (f next)) in
--        if check combined
--        then Just (index, next, prev) --prev is the value of the fold before satisfying pred
--        else mapAndFoldUntilHelper f fold combined check rest (index + 1)
