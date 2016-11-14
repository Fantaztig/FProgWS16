>module Aufgabe4 where

Tree und Order Definitionen

>data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Eq,Ord,Show)
	
>data Order = Up | Down deriving (Eq,Show)
	
Erzeuge einen leeren Baum
	
>nil :: Tree a
>nil = Nil

Pruefe ob Baum Nil ist

>isNilTree :: Tree a -> Bool
>isNilTree Nil = True
>isNilTree _ = False

Pruefe ob Baum nicht Nil ist

>isNodeTree :: Tree a -> Bool
>isNodeTree x = not (isNilTree x)

Gebe Linken Unterbaum zurueck

>leftSubTree :: Tree a -> Tree a
>leftSubTree Nil = error "Empty Tree as Argument"
>leftSubTree (Node a (x) (_)) = x

Gebe rechten Unterbaum zurueck

>rightSubTree :: Tree a -> Tree a
>rightSubTree Nil = error "Empty Tree as Argument"
>rightSubTree (Node a (_) (x)) = x

Gebe Wert des obersten Knotens des Baums zurueck

>treeValue :: Tree a -> a
>treeValue Nil = error "Empty Tree as Argument"
>treeValue (Node x (_) (_)) = x

Pruefe ob Wert im Baum enthalten

>isValueOf :: Eq a => a -> Tree a -> Bool
>isValueOf x Nil = False
>isValueOf a (Node x (y) (z))
>	|x == a = True
>	|otherwise = (isValueOf a y) || (isValueOf a z)
	
Pruefe ob Baum ein BST ist	
	
>isOrderedTree :: Ord a => Tree a -> Bool
>isOrderedTree x = (isSorted (treeToList x))

Erstelle Liste aus Values der Knoten in In-Order Reihenfolge

>treeToList :: Tree a -> [a]
>treeToList Nil = []
>treeToList x = (treeToList (leftSubTree x)) ++ [treeValue x] ++ (treeToList (rightSubTree x))
	
Pruefe ob Liste aufsteigend sortiert ist	
	
>isSorted :: Ord a => [a] -> Bool
>isSorted [] = True
>isSorted [x] = True
>isSorted (x:y:xs) = x < y && isSorted(y:xs)

Fuege Wert in den Baum ein

>insert :: Ord a => a -> Tree a -> Tree a
>insert x Nil = (Node x Nil Nil)
>insert v node@(Node x y z)
>	|not (isOrderedTree node) = error "Argument Tree not Ordered"
>	|x == v = node
>	|v < x = (Node x (insert v y) z)
>	|otherwise = (Node x y (insert v z))

Entferne Wert aus dem Baum

>delete :: Ord a => a -> Tree a -> Tree a
>delete _ Nil = Nil
>delete v node@(Node x Nil Nil)
>	|x == v = Nil
>	|otherwise = node
>delete v node@(Node x y z)
>	|not (isOrderedTree node) = error "Argument Tree not Ordered"
>	|v < x = (Node x (delete v y) z)
>	|v > x = (Node x y (delete v z))
>	|y == Nil = z
>	|z == Nil = y
>	|otherwise = (Node (predecessor) (delete predecessor y) z)
>	where
>		predecessor = findBiggestVal y

Gebe kleinsten Wert im Baum zurueck

>findBiggestVal :: Ord a => Tree a -> a
>findBiggestVal (Node x y Nil) = x
>findBiggestVal (Node x y z) = findBiggestVal z

Erstelle Liste aus Baum, entweder auf- oder absteigend Sortiert

>flatten :: Ord a => Order -> Tree a -> [a]
>flatten _ Nil = []
>flatten mode x
>	|not (isSorted (flattened)) = error "Argument Tree not Ordered"
>	|mode == Up = flattened
>	|otherwise = reverse flattened
>	where flattened = (treeToList x)

Gebe Laenge des laengsten Pfads im Baum aus

>maxLength :: Tree a -> Int
>maxLength Nil = 0
>maxLength x
>	|leftLength > rightLength = leftLength + 1
>	|otherwise = rightLength + 1
>	where 
>		leftLength = (maxLength (leftSubTree x))
>		rightLength = (maxLength (rightSubTree x))

Gebe Laenge des kuerzesten Pfads im Baum aus

>minLength :: Tree a -> Int
>minLength Nil = 0
>minLength x
>	|leftLength > rightLength = rightLength + 1
>	|otherwise = leftLength + 1
>	where 
>		leftLength = (minLength (leftSubTree x))
>		rightLength = (minLength (rightSubTree x))

Gebe den Absolutwert der Differenz der Min- und Maxlaenge aus

>balancedDegree :: Tree a -> Int
>balancedDegree x = abs ((maxLength x) - (minLength x))



