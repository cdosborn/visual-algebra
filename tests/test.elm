import Vector as V
import Constants as C
import Expr as E
import Ui as U
import ElmTest.Assertion (assert, assertEqual) 
import ElmTest.Test (suite, test, Test)
import ElmTest.Runner.Element (runDisplay)


{-
    Test suite for vector library in visual-algebra project
    TODO:
    pull out vectors, replace as variables
-}

main : Element
main = runDisplay (suite "Test suite for vector library in visual-algebra project" [tests, tests2, tests3])

tests : Test
tests = suite "Three dimensional vector functions" 
      [ test "2 parallel vectors are linearly dependent" independent1
      , test "2 equivalent vectors form dependent set" independent2
      , test "2 parallel vectors are linearly dependent" getBasis1 
      , test "3 parallel vectors are linearly dependent" getBasis2 
      , test "order of vectors doesn't change linearly dependence" getBasis3 
      , test "the span of two parallel vectors is a line" eval1
      , test "the span of two parallel vectors and antiparallel is a plane" eval2
      , test "vector projection onto a plane" project1 
      , test "vector projection onto a parallel vector" project2 
      , test "orthagonal basis 3 dimensions" getOrthoBasis1
      ] 

tests2 = suite "Expression methods" 
      [ test "replace the tail of an expression" replace1 
      , test "replace the tail of an expression" replace2 
      , test "replace the id in a node" replacePosWithInt1 
      , test "generates correct toString" toString1
      , test "mapAndFoldUntil" mapAndFoldUntil1
      , test "mapAndFoldUntil" mapAndFoldUntil2 ]


tests3 = suite "Ui methods" 
    [ test "basic" compressHistory1 
    , test "basic" compressHistory2 
    , test "basic" historyLength1  
    , test "basic" historyLength2 ] 

independent1 = assert (False == (V.independent [V.Vector 1 1 1, V.Vector 2 2 2]))
independent2 = assert (False == (V.independent [V.Vector 3 3 3,V.Vector 3 3 3]))
getBasis1 = assertEqual [V.Vector 1 1 1] (V.getBasis [V.Vector 1 1 1, V.Vector 2 2 2] [])
getBasis2 = assertEqual [V.Vector 1 1 1] (V.getBasis [V.Vector 1 1 1, V.Vector 2 2 2,V.Vector 3 3 3] [])
getBasis3 = assertEqual [V.Vector 2 1 2, V.Vector 3 3 3] (V.getBasis [V.Vector 3 3 3, V.Vector 2 1 2,V.Vector 3 3 3] [])
eval1 = assertEqual (V.Line (V.Vector 1 1 1)) (V.eval (V.Span [V.Vector 1 1 1, V.Vector 2 2 2]))
eval2 = assertEqual (V.Plane [V.Vector 1 1 0, V.Vector 1 1 1] ) (V.eval (V.Span [V.Vector 1 1 1, V.Vector 2 2 2, V.Vector 1 1 0]))
project1 = assert (case (V.project (V.Vector 1 1 1) (V.Plane [V.Vector 1 0 0, V.Vector 0 1 0])) of
                  V.Vector a b c -> c == 0
                  _ -> False)

project2 = assertEqual (V.Vector 1 1 1) (V.project (V.Vector 1 1 1) (V.Vector 2 2 2))
getOrthoBasis1 = 
    assertEqual 0 
    (let basis = V.getOrthoBasis [V.Vector 0 3 5, V.Vector 0 2 0, V.Vector 1 0 0]
         b1    = head basis
         b2    = head (tail basis)
         b3    = last basis
    in (b1 `V.dot` b2) + (b2 `V.dot` b3) + (b1 `V.dot` b3))

replace1 = assertEqual (E.Node 0 [E.Leaf 0, E.Leaf 1, E.Leaf 5]) 
                       (E.replace 1 (E.Node 0 [E.Leaf 3, E.Leaf 1, E.Leaf 5]) (E.Leaf 0))
replace2 = assertEqual (E.Node 0 [E.Leaf 0, E.Leaf 0]) 
                       (E.replace 2 (E.Node 0 [E.Leaf 0, E.Leaf 5]) (E.Leaf 0))
replace3 = assertEqual (E.Node 0 [E.Empty, E.Empty]) 
                       (E.replace 2 (E.Node 0 [E.Leaf 0, E.Leaf 5]) (E.Leaf 0))


mapAndFoldUntil1 = assertEqual 
                   (Just (0,E.Node 0 [E.Leaf 0, E.Leaf 5],0)) 
                   (E.mapAndFoldUntil E.count (+) 0 (\num -> num >= 2) [E.Node 0 [E.Leaf 0, E.Leaf 5]])

mapAndFoldUntil2 = assertEqual 
                   (Just (1,E.Node 2 [],2)) 
                   (E.mapAndFoldUntil E.count (+) 0 (\num -> num >= 3) [E.Node 1 [E.Leaf 0], E.Node 2 [], E.Node 0 [E.Leaf 0, E.Leaf 5]])

replacePosWithInt1 = assertEqual (E.Node 0 [E.Leaf 0, E.Leaf 5])
                                 (E.replacePosWithInt 2 (E.Node 0 [E.Leaf 0, E.Leaf 7]) 5)

toString1 = assertEqual "add( subtract( a ), project(  ), add( a, f ) )"
                        (E.toString C.funs C.vars (E.Node 0 [E.Node 1 [E.Leaf 0], E.Node 2 [], E.Node 0 [E.Leaf 0, E.Leaf 5]]))

compressHistory1 = assertEqual ([], [U.Fun 0 U.Available])
                               (U.compressHistory [U.Fun 0 U.Available])

compressHistory2 = assertEqual ([U.Fun 0 U.Available, U.Fun 0 U.Available], [])
               (U.compressHistory [U.Fun 0 U.Available, U.Fun 0 U.Available, U.Meta 1 U.Available, U.Fun 0 U.Available])

historyLength1 = assertEqual 2 
                             (U.historyLength [U.Fun 0 U.Available, U.Fun 0 U.Available, U.Meta 1 U.Available])
                              
historyLength2 = assertEqual 4
                     (U.historyLength [U.Fun 0 U.Available, U.Fun 0 U.Available, U.Meta 1 U.Available, U.Fun 0 U.Available])

