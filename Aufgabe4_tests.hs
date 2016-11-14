import Aufgabe4
import Test.HUnit




emptyTree = nil


--https://en.wikipedia.org/wiki/Binary_search_tree#/media/File:Binary_search_tree.svg
treeDepth0ordered = (Node 8 Nil Nil)
treeDepth1ordered = (Node 8 (Node 3 Nil Nil) (Node 10 Nil Nil))
treeDepth2ordered = (Node 8 (Node 3 (Node 1 Nil Nil) (Node 6 Nil Nil)) (Node 10 Nil (Node 14 Nil Nil)))
treeDepth3ordered = (Node 8 (Node 3 (Node 1 Nil Nil) (Node 6 (Node 4 Nil Nil) (Node 7 Nil Nil))) (Node 10 Nil (Node 14 (Node 13 Nil Nil) Nil)))


treeDepth1unordered = (Node 8 (Node 3 Nil Nil) (Node 7 Nil Nil))
treeDepth2unordered = (Node 8 (Node 3 (Node 1 Nil Nil) (Node 6 Nil Nil)) (Node 7 Nil (Node 10 Nil Nil)))
treeDepth3unordered = (Node 8 (Node 3 (Node 1 Nil Nil) (Node 6 (Node 7 Nil Nil) (Node 7 Nil Nil))) (Node 10 Nil (Node 14 (Node 13 Nil Nil) Nil)))


treeDuplicateValue = (Node 8 (Node 3 (Node 3 Nil Nil) Nil) Nil)






test1 = TestLabel "Test isNilTree + isNodeTree + leftSubTree + rightSubTree + treeValue" $ ( 
  TestList [
    TestCase $ assertEqual "isNilTree emptyTree -> True"            (True)  (isNilTree emptyTree),
    TestCase $ assertEqual "isNilTree treeDepth0ordered -> False"   (False) (isNilTree treeDepth0ordered),
    
    TestCase $ assertEqual "isNodeTree emptyTree -> False"          (False) (isNodeTree emptyTree),
    TestCase $ assertEqual "isNodeTree treeDepth0ordered -> True"   (True)  (isNodeTree treeDepth0ordered),
    
    TestCase $ assertEqual "leftSubTree emptyTree -> error: Empty Tree as Argument" (treeDepth0ordered) (leftSubTree emptyTree),
    TestCase $ assertEqual "leftSubTree treeDepth0ordered -> emptyTree"             (emptyTree)         (leftSubTree treeDepth0ordered),
    TestCase $ assertEqual "leftSubTree treeDepth1ordered -> (Node 3 Nil Nil)"      ((Node 3 Nil Nil))  (leftSubTree treeDepth1ordered),
    
    TestCase $ assertEqual "rightSubTree emptyTree -> error: Empty Tree as Argument"    (treeDepth0ordered) (rightSubTree nil),
    TestCase $ assertEqual "rightSubTree treeDepth0ordered -> emptyTree"                (emptyTree)         (rightSubTree treeDepth0ordered),
    TestCase $ assertEqual "rightSubTree treeDepth1ordered -> (Node 10 Nil Nil)"        ((Node 10 Nil Nil)) (rightSubTree treeDepth1ordered),
    
    TestCase $ assertEqual "treeValue emptyTree -> error: Empty Tree as Argument"   (treeDepth0ordered) (treeValue nil),
    TestCase $ assertEqual "treeValue treeDepth0ordered -> 8"                       (8)                 (treeValue treeDepth0ordered),
    TestCase $ assertEqual "treeValue treeDepth1ordered -> 8"                       (8)                 (treeValue treeDepth1ordered)
  ])


test2 = TestLabel "Test isValueOf" $ ( 
  TestList [
    TestCase $ assertEqual "isValueOf 1 emptyTree -> False"             (False) (isValueOf 1 emptyTree),
    TestCase $ assertEqual "isValueOf 1 treeDepth0ordered -> False"     (False) (isValueOf 1 treeDepth0ordered),
    TestCase $ assertEqual "isValueOf 8 treeDepth0ordered -> True"      (True)  (isValueOf 8 treeDepth0ordered),
    TestCase $ assertEqual "isValueOf 3 treeDepth1ordered -> True"      (True)  (isValueOf 3 treeDepth1ordered),
    TestCase $ assertEqual "isValueOf 10 treeDepth1ordered -> True"     (True)  (isValueOf 10 treeDepth1ordered),
    TestCase $ assertEqual "isValueOf 13 treeDepth3ordered -> True"     (True)  (isValueOf 13 treeDepth3ordered),
    TestCase $ assertEqual "isValueOf 16 treeDepth3ordered -> False"    (False) (isValueOf 16 treeDepth3ordered),
    TestCase $ assertEqual "isValueOf 3 treeDuplicateValue -> True"     (True)  (isValueOf 3 treeDuplicateValue)
  ])


test3 = TestLabel "Test isOrderedTree" $ ( 
  TestList [
    --TestCase $ assertEqual "isOrderedTree emptyTree -> True"            (True) (isOrderedTree emptyTree),
    TestCase $ assertEqual "isOrderedTree treeDepth0ordered -> True"    (True) (isOrderedTree treeDepth0ordered),
    TestCase $ assertEqual "isOrderedTree treeDepth1ordered -> True"    (True) (isOrderedTree treeDepth1ordered),
    TestCase $ assertEqual "isOrderedTree treeDepth2ordered -> True"    (True) (isOrderedTree treeDepth2ordered),
    TestCase $ assertEqual "isOrderedTree treeDepth3ordered -> True"    (True) (isOrderedTree treeDepth3ordered),
    
    TestCase $ assertEqual "isOrderedTree treeDepth1unordered -> False" (False) (isOrderedTree treeDepth1unordered),
    TestCase $ assertEqual "isOrderedTree treeDepth2unordered -> False" (False) (isOrderedTree treeDepth2unordered),
    TestCase $ assertEqual "isOrderedTree treeDepth3unordered -> False" (False) (isOrderedTree treeDepth3unordered),


    TestCase $ assertEqual "isOrderedTree treeDuplicateValue -> False"  (False) (isOrderedTree treeDuplicateValue)
  ])


test4 = TestLabel "Test insert" $ ( 
  TestList [
    TestCase $ assertEqual "inserting into a non-ordered Tree -> error: Argument Tree not Ordered" (emptyTree) (insert 1 treeDepth1unordered),
    
    TestCase $ assertEqual "insert existing Node -> no change" ((Node 8 Nil Nil)) (insert 8 treeDepth0ordered),
    
    TestCase $ assertEqual "insert 1 treeDepth0ordered -> (Node 8 (Node 1 Nil Nil) Nil)"  ((Node 8 (Node 1 Nil Nil) Nil)) (insert 1 treeDepth0ordered),
    TestCase $ assertEqual "insert 9 treeDepth0ordered -> (Node 8 Nil (Node 9 Nil Nil))"  ((Node 8 Nil (Node 9 Nil Nil))) (insert 9 treeDepth0ordered),


    TestCase $ assertEqual "insert (-1) treeDepth3ordered -> "  ((Node 8 (Node 3 (Node 1 (Node (-1) Nil Nil) Nil) (Node 6 (Node 4 Nil Nil) (Node 7 Nil Nil))) (Node 10 Nil (Node 14 (Node 13 Nil Nil) Nil))))   (insert (-1) treeDepth3ordered),
    TestCase $ assertEqual "insert 5 treeDepth3ordered -> "     ((Node 8 (Node 3 (Node 1 Nil Nil) (Node 6 (Node 4 Nil (Node 5 Nil Nil)) (Node 7 Nil Nil))) (Node 10 Nil (Node 14 (Node 13 Nil Nil) Nil))))      (insert 5 treeDepth3ordered),
    TestCase $ assertEqual "insert 12 treeDepth3ordered -> "    ((Node 8 (Node 3 (Node 1 Nil Nil) (Node 6 (Node 4 Nil Nil) (Node 7 Nil Nil))) (Node 10 Nil (Node 14 (Node 13 (Node 12 Nil Nil) Nil) Nil))))     (insert 12 treeDepth3ordered)
  ])


test5 = TestLabel "Test delete" $ ( 
  TestList [
    TestCase $ assertEqual "deleting from a non-ordered Tree -> error: Argument Tree not Ordered"  (emptyTree) (delete 1 treeDepth1unordered),
    
    TestCase $ assertEqual "delete non-existing Node -> no change"  (emptyTree)         (delete 8 emptyTree),
    TestCase $ assertEqual "delete non-existing Node -> no change"  (treeDepth0ordered) (delete 1 treeDepth0ordered),
    
    TestCase $ assertEqual "no children -> remove node"  (emptyTree)        (delete 8 treeDepth0ordered),
    TestCase $ assertEqual "no children -> remove node"  ((Node 8 (Node 3 (Node 1 Nil Nil) (Node 6 Nil Nil)) (Node 10 Nil Nil)))  (delete 14 treeDepth2ordered),
    
    TestCase $ assertEqual "only left child -> remove node and replace it with child"  ((Node 8 (Node 3 (Node 1 Nil Nil) (Node 6 Nil Nil)) (Node 14 Nil Nil))) (delete 10 treeDepth2ordered),
    TestCase $ assertEqual "only right child -> remove node and replace it with child"  ((Node 8 (Node 3 (Node 1 Nil Nil) (Node 6 (Node 4 Nil Nil) (Node 7 Nil Nil))) (Node 10 Nil (Node 13 Nil Nil)))) (delete 14 treeDepth3ordered),


    TestCase $ assertEqual "both children -> replace with in-order predecessor"  ((Node 3 Nil (Node 10 Nil Nil))) (delete 8 treeDepth1ordered),
    TestCase $ assertEqual "both children -> replace with in-order predecessor"  ((Node 8 (Node 1 Nil (Node 6 Nil Nil)) (Node 10 Nil (Node 14 Nil Nil)))) (delete 3 treeDepth2ordered),
    TestCase $ assertEqual "both children -> replace with in-order predecessor"  ((Node 7 (Node 3 (Node 1 Nil Nil) (Node 6 (Node 4 Nil Nil) Nil)) (Node 10 Nil (Node 14 (Node 13 Nil Nil) Nil)))) (delete 8 treeDepth3ordered)
  ])


test6 = TestLabel "Test flatten" $ ( 
  TestList [
    TestCase $ assertEqual "flatten a non-ordered Tree -> error: Argument Tree not Ordered" ([]) (flatten Up treeDepth1unordered),
    
    --TestCase $ assertEqual "flatten emptyTree -> []"    ([])    (flatten Up emptyTree),
    
    TestCase $ assertEqual "flatten treeDepth0ordered -> "   ([8])                       (flatten Up treeDepth0ordered),
    TestCase $ assertEqual "flatten treeDepth1ordered -> "   ([3,8,10])                  (flatten Up treeDepth1ordered),
    TestCase $ assertEqual "flatten treeDepth2ordered -> "   ([1,3,6,8,10,14])           (flatten Up treeDepth2ordered),
    TestCase $ assertEqual "flatten treeDepth3ordered -> "   ([1,3,4,6,7,8,10,13,14])    (flatten Up treeDepth3ordered),


    TestCase $ assertEqual "flatten treeDepth0ordered -> "   ([8])                       (flatten Down treeDepth0ordered),
    TestCase $ assertEqual "flatten treeDepth1ordered -> "   ([10,8,3])                  (flatten Down treeDepth1ordered),
    TestCase $ assertEqual "flatten treeDepth2ordered -> "   ([14,10,8,6,3,1])           (flatten Down treeDepth2ordered),
    TestCase $ assertEqual "flatten treeDepth3ordered -> "   ([14,13,10,8,7,6,4,3,1])    (flatten Down treeDepth3ordered)
  ])


test7 = TestLabel "Test maxLength + minLength + balancedDegree" $ ( 
  TestList [
    TestCase $ assertEqual "maxLength emptyTree -> 0"           (0) (maxLength emptyTree),
    TestCase $ assertEqual "maxLength treeDepth0ordered -> 0"   (1) (maxLength treeDepth0ordered),
    TestCase $ assertEqual "maxLength treeDepth1ordered -> 1"   (2) (maxLength treeDepth1ordered),
    TestCase $ assertEqual "maxLength treeDepth2ordered -> 2"   (3) (maxLength treeDepth2ordered),
    TestCase $ assertEqual "maxLength treeDepth3ordered -> 3"   (4) (maxLength treeDepth3ordered),
    TestCase $ assertEqual "maxLength treeDepth1unordered -> 1" (2) (maxLength treeDepth1unordered),
    TestCase $ assertEqual "maxLength treeDepth2unordered -> 2" (3) (maxLength treeDepth2unordered),
    TestCase $ assertEqual "maxLength treeDepth3unordered -> 3" (4) (maxLength treeDepth3unordered),
    TestCase $ assertEqual "maxLength treeDuplicateValue -> 2"  (3) (maxLength treeDuplicateValue),
    
    TestCase $ assertEqual "minLength emptyTree -> 0"           (0) (minLength emptyTree),
    TestCase $ assertEqual "minLength treeDepth0ordered -> 0"   (1) (minLength treeDepth0ordered),
    TestCase $ assertEqual "minLength treeDepth1ordered -> 1"   (2) (minLength treeDepth1ordered),
    TestCase $ assertEqual "minLength treeDepth2ordered -> 2"   (2) (minLength treeDepth2ordered),
    TestCase $ assertEqual "minLength treeDepth3ordered -> 2"   (2) (minLength treeDepth3ordered),
    TestCase $ assertEqual "minLength treeDepth1unordered -> 1" (2) (minLength treeDepth1unordered),
    TestCase $ assertEqual "minLength treeDepth2unordered -> 2" (2) (minLength treeDepth2unordered),
    TestCase $ assertEqual "minLength treeDepth3unordered -> 3" (2) (minLength treeDepth3unordered),
    TestCase $ assertEqual "minLength treeDuplicateValue -> 2"  (1) (minLength treeDuplicateValue),
    
    TestCase $ assertEqual "balancedDegree emptyTree -> 0"              (0) (balancedDegree emptyTree),
    TestCase $ assertEqual "balancedDegree treeDepth0ordered -> 0"      (0) (balancedDegree treeDepth0ordered),
    TestCase $ assertEqual "balancedDegree treeDepth1ordered -> 0"      (0) (balancedDegree treeDepth1ordered),
    TestCase $ assertEqual "balancedDegree treeDepth2ordered -> 0"      (1) (balancedDegree treeDepth2ordered),
    TestCase $ assertEqual "balancedDegree treeDepth3ordered -> 1"      (2) (balancedDegree treeDepth3ordered),
    TestCase $ assertEqual "balancedDegree treeDepth1unordered -> 0"    (0) (balancedDegree treeDepth1unordered),
    TestCase $ assertEqual "balancedDegree treeDepth2unordered -> 0"    (1) (balancedDegree treeDepth2unordered),
    TestCase $ assertEqual "balancedDegree treeDepth3unordered -> 1"    (2) (balancedDegree treeDepth3unordered),
    TestCase $ assertEqual "balancedDegree treeDuplicateValue -> 0"     (2) (balancedDegree treeDuplicateValue)
  ])


main = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7]