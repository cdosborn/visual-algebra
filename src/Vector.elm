module Vector where
import Text as T
import Debug (log)

{-
    TODO:
    drawVector ignores units?
    eval is incomplete, it needs to eval its argument before evaluating its expression
        this will at least effect how V.span [] is handled in Ui.elm
    clean-up sorting routine, especially draw method, handle order, unit vectors
    annotate avg/add tests
    getBasis needs a helper, shouldn't need to pass [] as an arg
-}

data Space = Abyss 
           | Vector Float Float Float 
           | Add Space Space
           | Subtract Space Space
           | Project Space Space 
           | Reject Space Space 
           | Unit Space 
           | Negate Space
           | Scale Space
           | Rotate Space Space
           | Trace Space

-- Pre: The space passed is composed of mutable (containing Scale/Rotate)
--      or Vector
-- Post: Returns Abyss if condition above is not met, otherwise returns
--       Scale/Rotate mutated by theta, ignores Vector
eval : Float -> Space -> Space 
eval theta s = let theta' = theta * 10 in
    case s of
    Vector a b c -> s
    Negate a -> scale (eval theta a) -1
    Unit a -> unit (eval theta a)
    Scale a -> scale (eval theta a) (cos theta')
    Rotate a b ->
        let a' = eval theta a
            b' = unit (eval theta b)
        in 
            case b' `dot` a' of
            Nothing -> Abyss
            Just dotp -> 
                (scale a' (cos theta')) 
                `add` (scale (b' `cross` a') (sin theta'))
                `add` (scale (scale b' dotp) (1 - (cos theta')))
    Add a b -> add (eval theta a) (eval theta b)
    Subtract a b -> subtract (eval theta a) (eval theta b)
    Project a b -> case (eval theta a) of
        Vector c d e -> (case (eval theta b) of
        Vector f g h -> (Vector c d e) `project` (Vector f g h)
        _ -> Abyss)
        _ -> Abyss
    Reject a b -> case (eval theta a) of
        Vector c d e -> (case (eval theta b) of
        Vector f g h -> (Vector c d e) `reject` (Vector f g h)
        _ -> Abyss)
        _ -> Abyss
    _ -> Abyss

    _ -> Abyss

modF a b =
   if a > b 
   then modF (a - b) b
   else a

sortSpaces : Float -> [(Space, Color, Maybe String)] -> [(Space, Color, Maybe String)]
sortSpaces theta spaces =
    let angle = theta `modF` (pi*2)
        quadrants = map addQuadrant spaces 
    in map (\(v,c,l,q) -> (v,c,l)) (sortWith (compareQuadrants angle) quadrants)

compareQuadrants angle (a,b,c,quad1) (d,e,f,quad2) =
    if | quad1 == quad2 -> compareSameQuadrant angle a d
       | quad1 <= 4 && quad2 >  4 -> GT
       | quad1 >  4 && quad1 <= 4 -> LT
       | True -> let (q1, q2) = ((quad1 `mod` 4) + 1,(quad2 `mod` 4) + 1) in -- on same quadrant level upper or lower 4
        if | angle < (pi*1/2) -> (if | q1 == 3 || q2 == 1 -> GT
                                     | q2 == 3 || q1 == 1 -> LT
                                     | True -> EQ)
           | angle < pi     -> (if | q1 == 2 || q2 == 4 -> GT
                                   | q2 == 2 || q1 == 4 -> LT
                                   | True -> EQ)
           | angle < pi*3/2 -> (if | q1 == 1 || q2 == 3 -> GT
                                   | q2 == 1 || q1 == 3 -> LT
                                   | True -> EQ)
           | angle < pi*2   -> (if | q1 == 4 || q2 == 2 -> GT
                                   | q2 == 4 || q1 == 2 -> LT
                                   | True -> EQ)
           | True -> EQ

compareSameQuadrant angle a b = EQ
--  case a of
--  Vector a1 a2 a3 -> (case b of
--  Vector b1 b2 b3 ->  

        
-- explodes if not passed a vector
addQuadrant (vec, col, mlabel) =
    case vec of
    Vector x y z -> 
    let q = 
        if z >= 0
        then 
            if | x >= 0 && y >= 0 -> 1
               | x <  0 && y >= 0 -> 2
               | x <  0 && y <  0 -> 3
               | x >= 0 && y <  0 -> 4
        else 
            if | x >= 0 && y >= 0 -> 5
               | x <  0 && y >= 0 -> 6
               | x <  0 && y <  0 -> 7
               | x >= 0 && y <  0 -> 8
    in (vec, col, mlabel, q)

---- Pre: takes list of spaces, ignoring non-vector spaces
---- Post: Returns the space represented by the span of vector spaces
--evalSpan : [Space] -> Space
--evalSpan spaces =
--    let vectors = filter (\space -> case space of 
--                                    Vector a b c -> True 
--                                    _ -> False) spaces
--    in case vectors of 
--        [] -> Abyss 
--        one::[] -> Line one
--        more -> 
--            let basis = getBasis more []
--                count = length basis 
--            in if | count == 1 -> Line (head basis)
--                  | count == 2 -> Plane basis
--                  | otherwise -> Volume

-- Pre: Assumes list of expressions
-- Post: Sorts vectors by z depth, other spaces are infront
sortGeoms : [(Space, Color)] -> [(Space, Color)]
sortGeoms geoms =
    sortWith (\(g1,c1) (g2,c2) -> case g1 of
        Vector a b c -> (case g2 of
        Vector d e f -> EQ  
        _ -> GT) 
        _ -> LT) geoms

-- Pre: Assumes 3 vector basis, in pair vector notation
-- Post: Returns a form of the space rendered with respect to the basis
draw : [(Float,Float)] -> Float -> Maybe String -> Color -> Space -> Form
draw basis units label col space = 
    case space of
    Vector a b c -> drawVector basis label col space 
    Scale a -> draw basis units label col a
    Rotate a b -> draw basis units label col a
--  Line a -> drawLine basis units lightCol a  
--  Plane vs -> drawPlane basis units lightCol vs 
--  Volume -> drawVolume basis units lightCol
    _ -> toForm empty

-- Pre: Assumes list of vectors
-- Post: Returns true for a linearly independent set, false otherwise
independent : [Space] -> Bool
independent vs = case vs of
    [] -> True
    one::[] -> True
    one::two::[] -> case one of
        Vector a b c -> case two of
        Vector d e f -> not ((a^2+b^2+c^2)*(d^2+e^2+f^2) == (a*d+b*e+c*f)^2)
    one::two::three::[] -> case one of
        Vector a b c -> case two of
        Vector d e f -> case three of
        Vector g h i -> not (((a*e*i)+(b*f*g)+(c*d*h)-((c*e*g)+(b*d*i)+(a*f*h))) == 0)
    _ -> False

-- Pre: Takes set of vectors, and existing basis, [] if not known
-- Post: Returns a basis from a set of vectors
getBasis : [Space] -> [Space] -> [Space]
getBasis vectors basis =
    case vectors of
    [] -> basis
    v::vs -> case basis of
        [] -> getBasis vs [v]
        b::[] -> if independent [v,b]
                 then getBasis vs (v::basis)
                 else getBasis vs basis
        b1::b2::[] -> if independent [v,b1,b2]
                      then [v,b1,b2]
                      else getBasis vs basis
        _ -> basis

---- Pre: Takes set of vectors
---- Post: Returns a perpendicular basis from a set of vectors
--getOrthoBasis : [Space] -> [Space]
--getOrthoBasis vectors =
--    let basis = getBasis vectors []
--    in case basis of
--        [] -> []
--        one::[] -> basis
--        one::two::[] -> [two, one `reject` two]
--        one::two::three::[] -> let partOfBasis = getOrthoBasis [one, two]  
--            in (three `reject` (Plane partOfBasis))::partOfBasis

cross : Space -> Space -> Space
cross a b =
    case a of
    Vector a1 a2 a3 -> (case b of
    Vector b1 b2 b3 -> 
        let c1 = (a2*b3 - a3*b2)
            c2 = (a3*b1 - a1*b3)
            c3 = (a1*b2 - a2*b1) 
        in Vector c1 c2 c3
    _ -> Abyss)
    _ -> Abyss

-- Pre: Assumes first can be projectable onto the second (ex. vectors onto planes | vectors)
-- Post: Returns the complement of the vector projection (rejection), returns Abyss if not vectors
reject : Space -> Space -> Space
reject v1 v2 = case v1 of
    Vector a b c -> v1 `subtract` (v1 `project` v2) 
    _ -> Abyss

-- Pre: Assumes first can be projectable onto the second (ex. vectors onto planes | vectors)
-- Post: Returns vector projection of first onto second, returns Abyss if not vectors
project : Space -> Space -> Space
project v1 v2 = case v1 of
    Vector a b c -> (case v2 of
    Vector d e f -> let dotp = v1 `dot` v2
                    in case dotp of
                    Nothing -> Abyss
                    Just dotp -> scale v2 (dotp/(d*d+e*e+f*f))
--  Plane vs -> let basis = getOrthoBasis vs
--                  b1 = head basis
--                  b2 = last basis
--              in (v1 `project` b1) `add` (v1 `project` b2)
    _ -> Abyss)
    _ -> Abyss

-- Pre: Assumes two arguments are Vector spaces
-- Post: Returns dot product of vectors, fails if not vectors
dot : Space -> Space -> Maybe Float
dot v1 v2 = case v1 of
    Vector a b c -> (case v2 of
    Vector d e f -> Just (a*d + b*e + c*f)
    _ -> Nothing)
    _ -> Nothing

-- Pre: Assumes two arguments are Vector spaces
-- Post: Returns vector difference of vectors, returns Abyss if not vectors
subtract : Space -> Space -> Space
subtract v1 v2 = case v1 of
    Vector a b c -> (case v2 of
    Vector d e f -> Vector (a-d) (b-e) (c-f)
    _ -> Abyss)
    _ -> Abyss

-- Pre: Assumes two arguments are Vector spaces
-- Post: Returns vector sum of vectors, returns Abyss if not vectors
add : Space -> Space -> Space
add v1 v2 = case v1 of
    Vector a b c -> (case v2 of
    Vector d e f -> Vector (a+d) (b+e) (c+f)
    _ -> Abyss)
    _ -> Abyss

-- Pre: Assumes first argument is a vector space
-- Post: Returns scaled vector, returns Abyss for non vector space
scale : Space -> Float -> Space
scale v s = case v of
    Vector a b c -> Vector (a*s) (b*s) (c*s)
    _ -> Abyss

-- Pre: Assumes first argument is a vector space
-- Post: Returns length of vector
distance : Space -> Maybe Float
distance v = case v of
    Vector a b c -> Just (sqrt(a*a+b*b+c*c))
    _ -> Nothing
    

unit : Space -> Space
unit v = 
    case distance v of
    Just dist -> (case v of
        Vector a b c -> scale v (1 / dist)
        _ -> Abyss)
    _ -> v


-- Pre: Assumes vector is a pair of floats
-- Post: Returns scaled vector, returns Abyss for non vector space
drawVector : [(Float, Float)] -> Maybe String -> Color -> Space -> Form
drawVector basis mlabel col space = 
    case space of
    Vector a b c ->
        let vec = basis |> zipWith (\c b -> scale' b c) [a,b,c]
                        |> foldr add' (0,0) 
            polar = toPolar vec
            theta = snd polar    
            vec' = fromPolar ((fst polar) + 2, theta)  --increase length to align click with head
            seg = segment (0,0) vec'
            base = subt' vec' (fromPolar (8, theta))
            arr1 = add' base (fromPolar (perp' 3.0 theta))
            arr2 = subt' base (fromPolar (perp' 3.0 theta))
            baseSeg = segment (0,0) base
            seg1 = segment vec' arr1
            seg2 = segment vec' arr2
            style = traced { defaultLine | width <- 2 , color <- col}
            strStyle = (\str -> (leftAligned (T.height 12 (monospace (toText str)))))
            label = 
                case mlabel of
                Nothing -> toForm empty 
                Just str -> move (fst vec', (snd vec') + 13) (toForm (strStyle str))
        in group [style seg1, style seg2, style seg, label]

drawLine : [(Float, Float)] -> Float -> Color -> Space -> Form
drawLine basis units col space = 
    case space of
    Vector a b c ->
        let vec = basis |> zipWith (\c b -> scale' b c) [a,b,c]
                        |> foldr add' (0,0) 
            largest = 100 * units * maximum (map (\b -> distance' (0,0) b) basis)
            polar = toPolar vec
            theta = snd polar    
            seg = segment (fromPolar (largest, theta)) (fromPolar (largest, theta + pi))
            style = traced { defaultLine | width <- 2 , color <- col}
    in (style seg)



-- Pre: Assumes basis has 3 vectors in float pair notation
-- Post: Returns the grid as dots determined by number of units
drawGrid : [(Float, Float)] -> Float  -> [Form]
drawGrid basis units = 
    let b1 = head basis
        b2 = head (tail basis)
        b3 = head (tail (tail  basis))
        b1s = [-units .. units]
        b2s = [-units .. units]
        b3s = [-units .. units]
        b1Points = map (\c -> scale' b1 c) b1s  
        b2Points = map (\c -> scale' b2 c) b2s  
        b3Points = map (\c -> scale' b3 c) b3s  
        allPoints = concatMap (\b3' -> map (\b12 -> add' b12 b3') (concatMap (\b1' -> map (\b2' -> add' b1' b2') b2Points ) b1Points)) b3Points
        allForms = map (\p -> move p (filled (greyscale 0.8) (circle  1.0))) allPoints
    in allForms

volumePoints basis units = 
    let b1 = head basis
        b2 = head (tail basis)
        b3 = head (tail (tail  basis))
        center = [ add' (scale' b1 -units) (scale' b2 -units) 
                 , add' (scale' b2 -units) (scale' b1 units)
                 , add' (scale' b1 units)  (scale' b2 units) 
                 , add' (scale' b2 units)  (scale' b1 -units)
                 ]
    in concatMap (\point -> [add' point (scale' b3 -units), add' point (scale' b3 units)]) center

-- Pre: Assumes basis has 3 vectors in float pair notation
-- Post: Returns max volume that could fit in basis grid
drawVolume : [(Float,Float)] -> Float -> Color -> Form
drawVolume basis units col = 
    let allPoints = volumePoints basis units
        outline = outsidePath' allPoints
    --in group (map (\p -> move p (filled black (circle  1.5))) outline)
    in filled col (polygon outline)

--drawPlane : [(Float,Float)] -> Float -> Color -> [Space] -> Form
--drawPlane basis units col vs = 
--    let plane = Plane (getOrthoBasis vs)
--        bs = [ Vector 1 1 1, Vector -1 -1 1, Vector 1 -1 1, Vector -1 1 1
--             , Vector 1 1 -1, Vector -1 -1 -1, Vector 1 -1 -1, Vector -1 1 -1]
--        projections = map (\b -> b `project` plane) bs
--        points = map (\v -> case v of
--                            Vector a b c -> basis |> zipWith (\c b -> scale' b (c*units)) [a,b,c]
--                                                  |> foldr add' (0,0)) projections
--        --boundary = volumePoints basis units 
--        --points' = filter (\p'-> or (map (\p -> p == p') boundary)) points
--        outline = outsidePath' points
--    in filled col (polygon outline)


--probably the worst blunder of code ever writtern
--filters out non vectors before computing avg of vs
avg vs = avgHelper (filter (\exp -> case exp of 
            Vector a b c -> True 
            _ -> False) vs) (Vector 0 0 0) 1.0



avgHelper vs v counter = 
    case vs of 
    [] -> scale v (1 / counter)
    one::more -> avgHelper more (add one v) (counter + 1) 


{- 
    2D Functions

    functionName' = blah blah
    function names end with an apostrophe in order to 
    distinguish between the vector space functions

-}

outsidePath' : [(Float, Float)] -> [(Float, Float)]
outsidePath' points = 
    let leastX = sortWith (\(x,y) (x', y') -> if | x > x' -> GT 
                                                 | not (x == x') -> LT
                                                 | y > y' -> GT
                                                 | y < y' -> LT
                                                 | y == y' -> EQ) points
    in (buildPath' leastX []) ++ (buildPath' (reverse leastX) [])
      
buildPath' : [(Float,Float)] -> [(Float,Float)] -> [(Float,Float)]
buildPath' points solutions =
    case points of
    [] -> solutions
    next::more -> case solutions of
                [] -> buildPath' more [next]
                one::[] -> buildPath' more (next::solutions) --dont start recursion until 2 solns
                new::old::rest -> 
                    let turn = toRight' old new next
                    in if | turn < 0 -> buildPath' points (old::rest)
                          | turn > 0 -> buildPath' more (next::solutions)
                          | otherwise -> buildPath' more (next::old::rest)
                



toRight' a b c = (det' (subt' c a) (subt' b a))
        
--trans' : Matrix -> Vector -> Vector
trans' mat vec =
    let x = fst vec
        y = snd vec
        a = fst (head mat)
        c = snd (head mat)
        b = fst (head (tail mat))
        d = snd (head (tail mat))
    in (x*a + b*y, x*c + d*y)

inv' mat = 
    let a = fst (head mat)
        c = snd (head mat)
        b = fst (head (tail mat))
        d = snd (head (tail mat))
        det' = a * d - b * c
    in if det' == 0
       then [(0,0),(0,0)]
       else [(d / det',-c / det'),(-b / det', a / det')]

det' v1 v2 =
    let a = fst v1 
        b = fst v2
        c = snd v1
        d = snd v2
    in a * d - b * c

normal' r theta vec = (1.0, theta)
perp' r theta = (r, theta + pi/2)
subt' vec vec2 = ((fst vec) - (fst vec2), (snd vec) - (snd vec2))
add' vec vec2 = ((fst vec) + (fst vec2), (snd vec) + (snd vec2))
distance' vec vec2 = fst (toPolar (subt' vec vec2))
length' vec = sqrt ((fst vec)^2 + (snd vec)^2)
scale' (x, y) c = (c * x, c * y)
rotate' vec theta = let pol = toPolar vec 
                    in fromPolar (fst pol, (snd pol) + theta)
--proj u v = scale' v ((dot u v) / ((len v) * (len v)))
--dot u v = (fst u) * (fst v) + (snd u) * (snd v)
--parallel u v = ((proj u v) == u) || ((proj u v) == (scale' u -1))
--len vec = distance' (0,0) vec
--toLength vec l =
--    let p = toPolar vec
--    in fromPolar (l, snd p)
pretty' v = map (\x -> (toFloat (round (10 * x))) / 10) v

----converts from string to [Maybe (Vector)]
--vecReg str = (toMatrix str)
--matReg str = toMatrix str
--
--toMatrix str = findMatches str |> map .submatches      --list of maybe str lists
--                               |> map toMaybeFloatList --maybe str list to maybe float lst
--                               |> map toMaybePairList  --maybe float lst to maybe pair
--
--rexp = "\\((-?[0-9]+\\.?[0-9]*),(-?[0-9]+\\.?[0-9]*)\\)"
--
--findMatches str =  find (AtMost 2) (regex rexp) str
--
--toMaybeFloatList lst = case lst of
--                       (Just fst)::(Just snd)::[] -> map String.toFloat [fst, snd]
--                       _ -> []
--               
--toMaybePairList lst = case lst of
--                      (Just fst)::(Just snd)::[] -> Just (fst, snd)
--                      _ -> Nothing
