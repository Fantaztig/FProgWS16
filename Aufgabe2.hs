-- David Schroeder 1226747 TU Wien Funktionale Programmierung WS 16 Uebung 2

facLst :: Integer -> [Integer]
facLst n = take (fromIntegral n+1) facStream

factsL :: Integer -> [Integer]
factsL n = reverse (take (fromIntegral n+1) facStream)

-- berechne n!
factorial :: Integer -> Integer
factorial n = product [1..n]

facStream :: [Integer]
facStream = [factorial x | x<-[0..]]