import Vector as V
import ElmTest.Assertion (assert, assertEqual) 
import ElmTest.Test (suite, test, Test)
import ElmTest.Runner.Element (runDisplay)


{-
    Test suite for vector library in visual-algebra project
    TODO:
    pull out vectors, replace as variables
-}

main : Element
main = runDisplay tests

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
getOrthoBasis1 = assertEqual 0 (let basis = V.getOrthoBasis [V.Vector 0 3 5, V.Vector 0 2 0, V.Vector 1 0 0]
                                    b1 = head basis
                                    b2 = head (tail basis)
                                    b3 = last basis
                                in (b1 `V.dot` b2) + (b2 `V.dot` b3) + (b1 `V.dot` b3))
