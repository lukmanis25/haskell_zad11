--Zadanie 11
module Main where


main :: IO ()
main = do print "Wpisz liczbe"
          n <- readLn
          print (solution (n :: Integer))

-- Rekurencyjnie sprawdza czy zadanie da sie rozwiazac dla n
-- jesli tak to daje pierwsze rozwiazanie z listy rozwiazan
-- jesli nie to szuka rozwiazania dla n-1
-- zwraca pusta liste gdy n zmaleje do 0
solution :: Integer -> [Integer]
solution n
 | n == 0 = []
 | null (listOfAccepted n) = solution (n-1)
 | otherwise = myHead (listOfAccepted n)

-- Funckja zwraca liste wszystkich czwórek które spełniają warunki zadania:
-- Warunki zadania:
-- liczby musza być czwórką pitagorejską (a^2 + b^2 +c^2 = d^2)
-- liczby sumują się do n (a+b+c+d = n)
-- dana czwórka musi być pierwotna czyli największy wspólny dzielnik równa się 1
-- Funckja dziala tak ze generuje wszystkie mozliwe czwórki gdzie na kazdy miejscu
-- może się znaleść liczba od 0 do (n-1), następnie wybiera z listy wszystkich czwórek
-- te które spełniają warunki zadania
listOfAccepted :: Integer -> [[Integer]]
listOfAccepted n = [ [a , b , c , d]  | a <- [1..(n-1)], b <- [1..(n-1)] ,
                       c <- [1..(n-1)], d <- [1..(n-1)], a + b + c + d == n
                       && a * a + b * b + c * c == d * d &&  greatestCommonDivisor a b c d == 1]

-- Zwraca największy wspólny dzielnik, gcd to funkcja z haskela która zwraca własnie największy wspólny dzielnik
greatestCommonDivisor :: Integer -> Integer -> Integer -> Integer -> Integer
greatestCommonDivisor a b c d = gcd a (gcd b (gcd c d))

-- Dziala jak zwykly head, czyli daje pierwszy element z listy
myHead :: [[Integer]] -> [Integer]
myHead (x:xs) = x
