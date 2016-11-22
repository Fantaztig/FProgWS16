import Aufgabe5
import Test.HUnit


import Control.Exception
import Control.Monad


-- Damit wir auch auf Exception Testen können


--instance Eq ErrorCall where
--   x == y = (show x) == (show y)   


assertException ex action
  = handleJust isWanted (const $ return ()) $ do
    action
    assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)


assertError ex f =
    TestCase $ assertException (ErrorCall ex) $ evaluate f


main = runTestTT $ TestList [test1, test3, test4, test5]


test1 = TestLabel "Test Eq, fromInteger" $ ( 
  TestList [
    TestCase $ assertEqual "Pos == Pos"     (Pos == Pos)    (True),
    TestCase $ assertEqual "Neg == Neg"     (Neg == Neg)    (True),
    TestCase $ assertEqual "Pos == Neg"     (Pos == Neg)    (False),
    TestCase $ assertEqual "Zero == Zero"   (Zero == Zero)  (True),
    TestCase $ assertEqual "One == One"     (One == One)    (True),
    TestCase $ assertEqual "Two == Two"     (Two == Two)    (True),
    TestCase $ assertEqual "Zero == One"    (Zero == One)   (False),
    TestCase $ assertEqual "Zero == Two"    (Zero == Two)   (False),
    TestCase $ assertEqual "One == Two"     (One == Two)    (False),
    TestCase $ assertEqual "Num (Neg,[Zero]) == Num (Pos,[Zero])"                             (Num (Neg,[Zero]) == Num (Pos,[Zero]))                                (True),
    TestCase $ assertEqual "(Num (Pos,[Zero,Zero,Zero])) == Num (Pos,[Zero])"                 (Num (Pos,[Zero,Zero,Zero]) == Num (Pos,[Zero]))                      (True),
    TestCase $ assertEqual "(Num (Neg,[Zero,Zero,Two,One,Zero])) == Num (Neg,[Two,One,Zero])" ((Num (Neg,[Zero,Zero,Two,One,Zero])) == Num (Neg,[Two,One,Zero]))      (True),
    TestCase $ assertEqual "fromInteger 10 ->(Num (Pos,[One,Zero,One])"                       (fromInteger (10))                                                      (Num (Pos,[One,Zero,One])),
    TestCase $ assertEqual "fromInteger -12482394"                                            (fromInteger (-12482394))                                             (Num (Neg,[Two,One,Two,One,One,One,Zero,One,One,One,Two,One,Two,Two,Zero])),   
    TestCase $ assertEqual "fromInteger (2^30) -> (Num (Pos,[Two,Two,Zero,Two,Two,One,One,One,Zero,Two,Two,Zero,One,Two,One,Two,Two,Zero,One]))" (fromInteger (2^30)) (Num (Pos,[Two,Two,Zero,Two,Two,One,One,One,Zero,Two,Two,Zero,One,Two,One,Two,Two,Zero,One])),
    TestCase $ assertEqual "fromInteger (-(2^30)) -> (Num (Neg,[Two,Two,Zero,Two,Two,One,One,One,Zero,Two,Two,Zero,One,Two,One,Two,Two,Zero,One]))" (fromInteger (-(2^30))) (Num (Neg,[Two,Two,Zero,Two,Two,One,One,One,Zero,Two,Two,Zero,One,Two,One,Two,Two,Zero,One]))
  ])


test3 = TestLabel "show" $ ( 
  TestList [
    TestCase $ assertEqual "show Pos" (show Pos)    ("+"),
    TestCase $ assertEqual "show Neg" (show Neg)    ("-"),
    TestCase $ assertEqual "show One" (show Zero)   ("0"),
    TestCase $ assertEqual "show One" (show One)    ("1"),
    TestCase $ assertEqual "show One" (show Two)    ("2"),
    TestCase $ assertEqual "show (Num (Neg,[Zero])) -> +0" (show (Num (Neg,[Zero]))) ("+0"),
    TestCase $ assertEqual "show (Num (Pos,[Zero,Zero,Zero])) -> +0" (show (Num (Pos,[Zero,Zero,Zero]))) ("+0"),
    TestCase $ assertEqual "show (Num (Neg,[Two,One,Two,One,One,One,Zero,One,One,One,Two,One,Two,Two,Zero])) -> -212111011121220" (show (Num (Neg,[Two,One,Two,One,One,One,Zero,One,One,One,Two,One,Two,Two,Zero]))) (show (-212111011121220)),
    TestCase $ assertEqual "show (Num (Neg,[Zero,Zero,Two,One,Zero])) -> -210" (show (Num (Neg,[Zero,Zero,Two,One,Zero]))) (show (-210)),
    TestCase $ assertEqual "show (Num (Pos,[Two,Two,Zero,Two,Two,One,One,One,Zero,Two,Two,Zero,One,Two,One,Two,Two,Zero,One])) -> 2202211102201212201" (show (Num (Pos,[Two,Two,Zero,Two,Two,One,One,One,Zero,Two,Two,Zero,One,Two,One,Two,Two,Zero,One]))) ("+" ++ show 2202211102201212201),
    TestCase $ assertEqual "show (Num (Neg,[Two,Two,Zero,Two,Two,One,One,One,Zero,Two,Two,Zero,One,Two,One,Two,Two,Zero,One])) -> -2202211102201212201" (show (Num (Neg,[Two,Two,Zero,Two,Two,One,One,One,Zero,Two,Two,Zero,One,Two,One,Two,Two,Zero,One]))) (show (-2202211102201212201))
  ])


test4 = TestLabel "Test (+), (*), (-) negate abs signum" $ (
    TestList [
        TestCase $ assertEqual "(+) (Num (Pos,[Two,One,Zero])) (Num (Pos,[Two,One,Zero]))" (Num (Pos,[One,One,Two,Zero])) ((+) (Num (Pos,[Two,One,Zero])) (Num (Pos,[Two,One,Zero]))),
        TestCase $ assertEqual "(+) (Num (Neg,[Two,One,Zero])) (Num (Neg,[Two,One,Zero]))" (Num (Neg,[One,One,Two,Zero])) ((+) (Num (Neg,[Two,One,Zero])) (Num (Neg,[Two,One,Zero]))),
        TestCase $ assertEqual "(+) (Num (Pos,[Two,One,Zero])) (Num (Neg,[Two,One,Zero]))" (Num (Pos,[Zero])) ((+) (Num (Pos,[Two,One,Zero])) (Num (Neg,[Two,One,Zero]))),
        TestCase $ assertEqual "(+) (Num (Pos,[Two,One,Zero])) (Num (Pos,[One,One,Zero,One]))" (Num (Pos,[Two,Zero,One,One])) ((+) (Num (Pos,[Two,One,Zero])) (Num (Pos,[One,One,Zero,One]))),
        TestCase $ assertEqual "(+) (Num (Pos,[Two,One,Zero])) (Num (Neg,[One,One,Zero,One]))" (Num (Neg,[One,Two,One])) ((+) (Num (Pos,[Two,One,Zero])) (Num (Neg,[One,One,Zero,One]))),
        TestCase $ assertEqual "(+) (Num (Neg,[Two,One,Zero])) (Num (Pos,[One,One,Zero,One]))" (Num (Pos,[One,Two,One])) ((+) (Num (Neg,[Two,One,Zero])) (Num (Pos,[One,One,Zero,One]))),
        
        TestCase $ assertEqual "(*) (Num (Pos,[One,Two])) (Num (Pos,[One,Two]))" (Num (Pos,[Two,Two,One])) ((*) (Num (Pos,[One,Two])) (Num (Pos,[One,Two]))),
        TestCase $ assertEqual "(*) (Num (Pos,[One,Two])) (Num (Neg,[One,Two]))" (Num (Neg,[Two,Two,One])) ((*) (Num (Pos,[One,Two])) (Num (Neg,[One,Two]))),
        TestCase $ assertEqual "(*) (Num (Neg,[One,Two])) (Num (Pos,[One,Two]))" (Num (Neg,[Two,Two,One])) ((*) (Num (Neg,[One,Two])) (Num (Pos,[One,Two]))),
        TestCase $ assertEqual "(*) (Num (Neg,[One,Two])) (Num (Neg,[One,Two]))" (Num (Pos,[Two,Two,One])) ((*) (Num (Neg,[One,Two])) (Num (Neg,[One,Two]))),


        TestCase $ assertEqual "(*) (Num (Pos,[One,Zero])) (Num (Pos,[One,One]))" (Num (Pos,[One,One,Zero])) ((*) (Num (Pos,[One,Zero])) (Num (Pos,[One,One]))),
        TestCase $ assertEqual "(*) (Num (Neg,[One,Zero])) (Num (Pos,[One,One]))" (Num (Neg,[One,One,Zero])) ((*) (Num (Neg,[One,Zero])) (Num (Pos,[One,One]))),
        TestCase $ assertEqual "(*) (Num (Pos,[One,Zero])) (Num (Neg,[One,One]))" (Num (Neg,[One,One,Zero])) ((*) (Num (Pos,[One,Zero])) (Num (Neg,[One,One]))),
        TestCase $ assertEqual "(*) (Num (Neg,[One,Zero])) (Num (Neg,[One,One]))" (Num (Pos,[One,One,Zero])) ((*) (Num (Neg,[One,Zero])) (Num (Neg,[One,One]))),
        


        TestCase $ assertEqual "(+) (int2num (2^10)) (fromInteger (2^10))"      ((2^11))         ((+) (int2num (2^10))   (fromInteger (2^10))),
        TestCase $ assertEqual "(*) (int2num (2^5)) (fromInteger (2^5))"        ((2^10))         ((*) (int2num (2^5))    (fromInteger (2^5))),
        TestCase $ assertEqual "(*) (int2num (2^10)) (fromInteger (7))"         (((2^10)*7))     ((*) (int2num (2^10))   (fromInteger (7))),
        TestCase $ assertEqual "(*) (int2num (7)) (fromInteger (2^10)) "        (((2^10)*7))     ((*) (int2num (7))      (fromInteger (2^10))),


        TestCase $ assertEqual "(-) (int2num (2^10)) (fromInteger (2^5))"       (((2^10)-(2^5))) ((-) (int2num (2^10)) (fromInteger (2^5))),
        TestCase $ assertEqual "(-) (int2num (2^10)) (fromInteger (7))"         (((2^10)-7))     ((-) (int2num (2^10)) (fromInteger (7))),
        TestCase $ assertEqual "(-) (int2num (2^10)) (fromInteger (2^10))"      ((0))            ((-) (int2num (2^10)) (fromInteger (2^10))),


        TestCase $ assertEqual "negate (Num (Neg,[Two,One,Zero])) -> (Num (Pos,[Two,One,Zero]))"    (negate (Num (Neg,[Two,One,Zero]))) (Num (Pos,[Two,One,Zero])),
        TestCase $ assertEqual "negate (Num (Pos,[Two,One,Zero])) -> (Num (Neg,[Two,One,Zero]))"    (negate (Num (Pos,[Two,One,Zero]))) (Num (Neg,[Two,One,Zero])),
        TestCase $ assertEqual "negate (Num (Neg,[Zero,Zero]) -> (Num (Pos,[Zero,Zero])"            (negate (Num (Neg,[Zero,Zero])))    (Num (Pos,[Zero,Zero])),


        TestCase $ assertEqual "abs (Num (Neg,[Two,One,Zero])) -> (Num (Pos,[Two,One,Zero]))"   (abs (Num (Neg,[Two,One,Zero]))) (Num (Pos,[Two,One,Zero])),
        TestCase $ assertEqual "abs (Num (Pos,[Two,One,Zero])) -> (Num (Pos,[Two,One,Zero]))"   (abs (Num (Pos,[Two,One,Zero]))) (Num (Pos,[Two,One,Zero])),
        TestCase $ assertEqual "abs (Num (Neg,[Zero,Zero]) -> (Num (Pos,[Zero,Zero])"           (abs (Num (Neg,[Zero,Zero])))    (Num (Pos,[Zero,Zero])),


        TestCase $ assertEqual "signum (Num (Neg,[Two,One,Zero])) -> (Num (Neg,[One]))"   (signum (Num (Neg,[Two,One,Zero]))) (Num (Neg,[One])),
        TestCase $ assertEqual "signum (Num (Pos,[Two,One,Zero])) -> (Num (Pos,[One]))"   (signum (Num (Pos,[Two,One,Zero]))) (Num (Pos,[One])),
        TestCase $ assertEqual "signum (Num (Neg,[Zero,Zero]) -> (Num (Pos,[Zero])"       (signum (Num (Neg,[Zero,Zero])))    (Num (Pos,[Zero]))
    ])


test5 = TestLabel "Test Ord" $ (
    TestList [
        TestCase $ assertEqual "Zero < One -> True"     (Zero < One) (True),
        TestCase $ assertEqual "Zero > One -> False"    (Zero > One) (False),
        TestCase $ assertEqual "Zero < Two -> True"     (Zero < Two) (True),
        TestCase $ assertEqual "Zero > Two -> False"    (Zero > Two) (False),
        TestCase $ assertEqual "One  < Two -> True"     (One  < Two) (True),
        TestCase $ assertEqual "One  > Two -> False"    (One  > Two) (False),
        TestCase $ assertEqual "int2num (-5) < fromInteger (1000) -> True" (int2num (-5) < fromInteger (1000)) (True), 
        TestCase $ assertEqual "int2num (-5) > fromInteger (1000) -> False" (int2num (-5) > fromInteger (1000)) (False)
    ])