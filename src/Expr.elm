module Expr where

{-
    TODO:
    factor out replace****
    setNodeExpr should throw error, see comment
-}

data Expr = Empty | Leaf Int | Node Int [Expr]
 
toString : [String] -> [String] -> Expr -> String
toString nodeIndex leafIndex expr = 
    case expr of
    Empty -> "_"
    Leaf i -> head (drop i leafIndex)
    Node i es ->
        let funName = head (drop i nodeIndex)
            children = join ", " (map (toString nodeIndex leafIndex) es)
        in funName ++ "( " ++ children ++ " )"

-- assumes format of lst
-- lst = [funId, argId ...]
-- lst must contain at least funId
fromList : [Int] -> Expr
fromList lst = 
    let funId = head lst
        args = tail lst 
        leaves = map (\int -> Leaf int) (tail lst)
    in Node funId leaves

setExpr : Expr -> Int -> Expr
setExpr expr num =
    case expr of
    Node _ exprs -> Node num exprs
    _ -> Leaf num -- if leaf/empty update/make leaf

-- ensures that the inserted node in replace,
-- uses the previous exprs arguments.
setNodeExpr : Expr -> Int -> Expr -> Expr
setNodeExpr expr num def =
    case expr of
    Node _ exprs -> Node num exprs
    _ -> def -- returns a specified default 


replace : Int -> Expr -> Expr -> Expr
replace pos tree exp =
    if pos == 0 
    then exp
    else case tree of -- search tree until pos == 0
        Node funId branches -> -- exploring a node
            case branches of 
            [] -> tree -- nowhere to go, invalid pos
            b::bs -> let triple = mapAndFoldUntil count (+) 0 (\num -> num >= pos) branches in
                case triple of
                Nothing -> tree -- pos isnt contained in tree
                Just (index, subTree, total) -> -- pos becomes 0 in the subtree which is branches[index]
                    let treeBef = take index branches
                        treeAft = drop (index + 1) branches
                    in Node funId (treeBef ++ [replace (pos - total - 1) subTree exp] ++ treeAft)
        _ -> tree -- nowhere to go (Leaf/Empty), invalid pos

replacePosWithInt : Int -> Expr -> Int -> Expr
replacePosWithInt pos tree num =
    if pos == 0 
    then setExpr tree num
    else case tree of -- search tree until pos == 0
        Leaf _ -> tree -- nowhere to go, invalid pos
        Node funId branches -> -- exploring a node
            case branches of 
            [] -> tree -- nowhere to go, invalid pos
            b::bs -> let triple = mapAndFoldUntil count (+) 0 (\num -> num >= pos) branches in
                case triple of
                Nothing -> tree -- pos isnt contained in tree
                Just (index, subTree, total) -> -- pos becomes 0 in the subtree which is branches[index]
                    let treeBef = take index branches
                        treeAft = drop (index + 1) branches
                    in Node funId (treeBef ++ [replacePosWithInt (pos - total - 1) subTree num] ++ treeAft)

replaceNode : Int -> Expr -> Int -> Expr -> Expr
replaceNode pos tree num def =
    if pos == 0 
    then setNodeExpr tree num def
    else case tree of -- search tree until pos == 0                                                        
        Leaf _ -> tree -- nowhere to go, invalid pos                                                       
        Node funId branches -> -- exploring a node                                                         
            case branches of                                                                               
            [] -> tree -- nowhere to go, invalid pos
            b::bs -> let triple = mapAndFoldUntil count (+) 0 (\num -> num >= pos) branches in
                case triple of
                Nothing -> tree -- pos isnt contained in tree
                Just (index, subTree, total) -> -- pos becomes 0 in the subtree which is branches[index]
                    let treeBef = take index branches
                        treeAft = drop (index + 1) branches
                    in Node funId (treeBef ++ [replaceNode (pos - total - 1) subTree num def] ++ treeAft)

-- counts the number of branches and leaves in a tree
count : Expr -> Int
count tree = 
    case tree of
    Empty -> 1
    Leaf _ -> 1
    Node _ branches ->
        branches |> map count
                 |> sum
                 |> (+) 1
                                                                                
-- returns Maybe (index, value) for a value in a list which satisfies predicate
first : ( a -> Bool) -> [a] -> Maybe (Int, a)
first check list = firstHelper check list 0

firstHelper check list index =
    case list of
    [] -> Nothing
    value::rest -> if (check value) then Just (index, value) else firstHelper check rest (index + 1)

-- folds over transformed values returning (index, value, accum) when the fold satisfies the predicate.
--      init is the initial value of the fold
--
--      Discussion:
--      The complexity comes from the desire to check the predicate of expensive transformations w/o
--      transforming all values. The unit function can be passed to change to a foldUntil method or 
--      a fold method which always returns the second value instead of the combination would result
--      in a mapUntil method.
mapAndFoldUntil f fold init check list =
    case list of
    [] -> Nothing
    front::rest -> let value = f front in
        if check value 
        then Just (0, front, init) 
        else mapAndFoldUntilHelper f fold value check rest 1

mapAndFoldUntilHelper f fold prev check list index =
    case list of
    [] -> Nothing
    next::rest -> let combined = (fold prev (f next)) in
        if check combined
        then Just (index, next, prev) --prev is the value of the fold before satisfying pred
        else mapAndFoldUntilHelper f fold combined check rest (index + 1)
