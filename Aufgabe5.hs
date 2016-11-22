module Aufgabe5 where

data Digit		= Zero | One | Two
type Digits		= [Digit]
data Sign		= Pos | Neg
newtype Numeral = Num (Sign,Digits)

instance Eq Digit where
	Zero == Zero = True
	One == One = True
	Two == Two = True
	_ == _ = False

instance Eq Sign where
	Pos == Pos = True
	Neg == Neg = True
	_ == _ = False

instance Eq Numeral where
	Num(x,y) == Num(z,v)
		|cx == cz && cy == cv = True
		|otherwise = False
		where 	{(Num(cx,cy)) = (canonize (Num(x,y)));
				(Num(cz,cv)) = (canonize (Num(z,v)))}
		
instance Show Digit where
	show Zero = "0"
	show One = "1"
	show Two = "2"
	
instance Show Sign where
	show Neg = "-"
	show Pos = "+"

instance Show Numeral where
	show (Num(x,y)) = (show v)++(concat ([show z|z<-w])) 
		where (Num(v,w)) = canonize (Num(x,y))
	
isInvalidNumeral :: Numeral -> Bool
isInvalidNumeral (Num (Pos,[])) = True
isInvalidNumeral (Num (Neg,[])) = True
isInvalidNumeral _ = False

canonize :: Numeral -> Numeral
canonize (Num (_,[Zero])) = (Num (Pos,[Zero]))
canonize (Num (x,Zero:xs)) = canonize (Num (x,xs))
canonize x
	|isInvalidNumeral x = error "Invalid Argument"
	|otherwise = x
	
instance Ord Digit where
	Two `compare` Two = EQ
	Two `compare` _ = GT
	_ `compare` Two = LT
	One `compare`  One = EQ
	One `compare`  _ = GT
	_ `compare`	 One = LT
	Zero `compare`	Zero = EQ
	
instance Ord Numeral where
	Num(_,[Zero]) `compare` Num(_,[Zero]) = EQ
	Num(Neg,_) `compare` Num(Pos,_) = LT
	Num(Pos,_) `compare` Num(Neg,_) = GT
	x `compare` y
		|(length cy) < (length cz) = less
		|(length cy) > (length cz) = more
		|(compare cy cz) == LT = less
		|(compare cy cz) == GT = more
		|otherwise = EQ
		where	{(Num(cx,cy)) = (canonize x);
				(Num(_,cz)) = (canonize y);
				less
					|cx == Neg = GT
					|otherwise = LT;
				more
					|cx == Neg = LT
					|otherwise = GT}
	
instance Num Numeral where
	fromInteger x = int2num x
	abs (Num(_,val)) = (Num(Pos,val))
	x + y = fromInteger((num2int x)+(num2int y))
	x - y = fromInteger((num2int x)-(num2int y))
	x * y = fromInteger((num2int x)*(num2int y))
	negate (Num(Pos,val)) = (Num(Neg,val))
	negate (Num(Neg,val)) = (Num(Pos,val))
	signum (Num(x,y))
		|cy == [Zero] = (Num(Pos,[Zero]))
		|cx == Neg = (Num(Neg,[One]))
		|otherwise = (Num(Pos,[One]))
		where (Num(cx,cy)) = (canonize (Num(x,y)))
	
int2num :: Integer -> Numeral
int2num x
	|x>0 = canonize(Num (Pos,(toBase3 (betrag x))))
	|otherwise = canonize(Num (Neg,(toBase3 (betrag x))))	
	
toBase3 :: Integer -> Digits
toBase3 0 = [Zero]
toBase3 x
	|x `mod` 3 == 2 = (toBase3 (div (x-2) 3)) ++ [Two]
	|x `mod` 3 == 1 = (toBase3 (div (x-1) 3)) ++ [One]
	|otherwise = (toBase3 (div x 3)) ++ [Zero]
	
betrag :: Integer -> Integer
betrag x
	|x >= 0 = x
	|otherwise = -x

num2int :: Numeral -> Integer
num2int n@(Num (v,xs))
	|(canonize n) /= n = num2int (canonize n)
	|v == Pos = fromBase3 xs
	|otherwise = -(fromBase3 xs)
	
fromBase3 :: Digits -> Integer
fromBase3 [] = 0
fromBase3 xs
	|(last xs) == Two = 2 + 3* (fromBase3 (init xs))
	|(last xs) == One = 1 + 3* (fromBase3 (init xs))
	|otherwise = 0 + 3* (fromBase3 (init xs))
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	