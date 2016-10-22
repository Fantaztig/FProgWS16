-- David Schroeder 1226747 TU Wien Funktionale Programmierung WS 16 Uebung 1


-- Aufgabe 1: Inverse Fakultaet bestimmen und falls existent ausgeben, ansonsten -1 ausgeben
invFac :: Integer -> Integer
invFac n 
    | n < 2 = -1
    | otherwise = facIndex 1 n

-- finde erstes n fuer das gilt: n! >= limit, gebe n aus für n! == limit, sonst -1
facIndex :: Integer -> Integer -> Integer
facIndex n limit
    |fac > limit = -1
    |fac == limit = n
    |fac < limit = facIndex (n+1) limit
    where fac = factorial n
    
-- berechne n!
factorial :: Integer -> Integer
factorial n = product [1..n]


--Aufgabe 2: Zahlenfolge aus String extrahieren
extractDigits :: String -> String
extractDigits input = [x | x <- input, x `elem` ['0'..'9']]


--Aufgabe 3: Zahlenfolge extrahieren und zu Integer konvertieren
convert :: String -> Integer
convert input
    |digits == "" = 0
    |otherwise = read digits
    where digits = extractDigits input
    
    
--Aufgabe 4: Die erste Primzahl mit Laenge n aus einem String bestimmen
findLeftMostPrime :: String -> Int -> Integer
findLeftMostPrime input n
    |primes == [] = 0
    |otherwise = head primes
    where primes = [x | x <- intsOfLengthN (extractDigits input) n, isPrime x]
    
--Sucht Zahlen der Laenge n in einem String
intsOfLengthN :: String -> Int -> [Integer]
intsOfLengthN input n
    |(length input < n) || (n < 1) = []
    |next < 10^(n-1) = intsOfLengthN (tail input) n
    |otherwise = next : intsOfLengthN (tail input) n
    where next = read(take n input)
    
--Check ob eine Zahl Prim ist
isPrime :: Integer -> Bool
isPrime n = primeFactors n == [1,n]

--Primfaktorzerlegung
primeFactors :: Integer -> [Integer]
primeFactors n = [x | x <- [1..n], mod n x == 0]
    
    
--Aufgabe 5: Alle Primzahlen in der Zahlenfolge eines Strings bestimmen 
findAllPrimes :: String -> Int -> [Integer]
findAllPrimes input n = [x | x <- intsOfLengthN (extractDigits input) n, isPrime x]