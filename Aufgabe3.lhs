Uebung 3 David Schroeder 1226747

>data Digit			= Zero | One | Two deriving (Eq,Enum,Show)
>type Digits		= [Digit]
>data Sign 			= Pos | Neg deriving (Eq,Show)
>newtype Numeral	= Num (Sign,Digits) deriving (Eq,Show)

Auf ungueltige Numerale pruefen

>isInvalidNumeral :: Numeral -> Bool
>isInvalidNumeral (Num (Pos,[])) = True
>isInvalidNumeral (Num (Neg,[])) = True
>isInvalidNumeral _ = False

In Kanonische Darstellung ueberfuehren

>canonize :: Numeral -> Numeral
>canonize (Num (_,[Zero])) = (Num (Pos,[Zero]))
>canonize (Num (x,Zero:xs)) = canonize (Num (x,xs))
>canonize x
>	|isInvalidNumeral x = error "Invalid Argument"
>	|otherwise = x

Integer in base 3 Numeral ueberfuehren

>int2num :: Integer -> Numeral
>int2num x
>	|x>0 = canonize(Num (Pos,(toBase3 (betrag x))))
>	|otherwise = canonize(Num (Neg,(toBase3 (betrag x))))
	
Interne Methode zur Umrechnung von base 10 zu base 3
	
>toBase3 :: Integer -> Digits
>toBase3 0 = [Zero]
>toBase3 x
>	|x `mod` 3 == 2 = (toBase3 (div (x-2) 3)) ++ [Two]
>	|x `mod` 3 == 1 = (toBase3 (div (x-1) 3)) ++ [One]
>	|otherwise = (toBase3 (div x 3)) ++ [Zero]

Berechne Betrag

>betrag :: Integer -> Integer
>betrag x
>	|x >= 0 = x
>	|otherwise = -x

Methode zur Umrechnung von base 3 nach base 10

>num2int :: Numeral -> Integer
>num2int n@(Num (v,xs))
>	|(canonize n) /= n = num2int (canonize n)
>	|v == Pos = fromBase3 xs
>	|otherwise = -(fromBase3 xs)

Interne Methode zur Umrechnung von base 3 nach base 10

>fromBase3 :: Digits -> Integer
>fromBase3 [] = 0
>fromBase3 xs
>	|(last xs) == Two = 2 + 3* (fromBase3 (init xs))
>	|(last xs) == One = 1 + 3* (fromBase3 (init xs))
>	|otherwise = 0 + 3* (fromBase3 (init xs))

Increase Funktion fuer base 3 Numerale

>inc :: Numeral -> Numeral
>inc (Num (Neg,[One])) = (Num (Pos,[Zero]))
>inc n@(Num (v,xs))
>	|isInvalidNumeral n = error "Invalid Argument"
>	|n /= (canonize n) = inc (canonize n)
>	|v == Neg = canonize(Num(Neg,decInternal(xs)))
>	|otherwise = canonize(Num(Pos,incInternal(xs)))

Decrease Funktion fuer base 3 Numerale

>dec :: Numeral -> Numeral
>dec (Num (Pos,[Zero])) = (Num (Neg,[One]))
>dec n@(Num (v,xs))
>	|isInvalidNumeral n = error "Invalid Argument"
>	|n /= (canonize n) = dec (canonize n)
>	|v == Neg = canonize(Num(Neg,incInternal(xs)))
>	|otherwise = canonize(Num(Pos,decInternal(xs)))
	
Interne increase Routine
	
>incInternal :: Digits -> Digits
>incInternal [] = [One]
>incInternal xs
>	|(last xs) == Two = (incInternal (init xs)) ++ [Zero]
>	|(last xs) == One = (init xs) ++ [Two]
>	|otherwise = (init xs) ++ [One]

Interne decrease Routine

>decInternal :: Digits -> Digits
>decInternal xs
>	|(last xs) == Two = (init xs) ++ [One]
>	|(last xs) == One = (init xs) ++ [Zero]
>	|otherwise = (decInternal (init xs)) ++ [Two]

Addition in base 3

>numAdd  :: Numeral -> Numeral -> Numeral
>numAdd (Num (_,[Zero])) ys@(Num (_,_)) = canonize(ys)
>numAdd xs@(Num (_,_)) (Num (_,[Zero])) = canonize(xs)
>numAdd xs@(Num (xv,_)) ys@(Num (yv,_))
>	|isInvalidNumeral xs || isInvalidNumeral ys = error "Invalid Argument"
>	|xv == Pos && yv == Neg = numAdd (dec xs) (inc ys)
>	|otherwise = numAdd (inc xs) (dec ys)

Multiplikation in base 3

>numMult :: Numeral -> Numeral -> Numeral
>numMult xs@(Num (xv,_)) ys@(Num (yv,_))
>	|isInvalidNumeral xs || isInvalidNumeral ys = error "Invalid Argument"
>	|xv == yv = numMultInternal xs ys (Num (Pos,[Zero]))
>	|otherwise = numMultInternal xs ys (Num (Neg,[Zero]))

Interne Routine zum Multiplizieren

>numMultInternal :: Numeral -> Numeral -> Numeral -> Numeral
>numMultInternal (Num (_,[Zero])) _ xs = xs
>numMultInternal xs xy xz = numMultInternal (dec xs) xy (numAdd xz xy)

Getestet mittels (((curryFlip strcat) "fant") "ele")

>curryFlip :: ((a,b) -> c) -> b -> a -> c
>curryFlip f x y = f (y,x)

Getestet mittels (uncurryFlip div) (2,4)

>uncurryFlip :: (a -> b -> c) -> (b,a) -> c
>uncurryFlip f (x,y) = f y x

Getestet mittels pairFlip(strcat) ("awa","awb")

>pairFlip :: ((a,b) -> c) -> (b,a) -> c
>pairFlip f (x,y) = f (y,x)

>strcat (x, y) = x ++ y 