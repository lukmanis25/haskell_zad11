--Zadanie 11
module Main where


main :: IO ()
main = do print "Wpisz liczbe"
          n <- readLn
          print (solution (n :: Integer))

solution :: Integer -> [Integer]
solution n
 | n == 0 = []
 | null (listOfAccepted n) = solution (n-1)
 | otherwise = myHead (listOfAccepted n)

listOfAccepted :: Integer -> [[Integer]]
listOfAccepted n = [ [a , b , c , d]  | a <- [1..(n-1)], b <- [1..(n-1)] ,
                       c <- [1..(n-1)], d <- [1..(n-1)], a + b + c + d == n
                       && a * a + b * b + c * c == d * d &&  greatestCommonDivisor a b c d == 1]

greatestCommonDivisor :: Integer -> Integer -> Integer -> Integer -> Integer
greatestCommonDivisor a b c d = gcd a (gcd b (gcd c d))

myHead :: [[Integer]] -> [Integer]
myHead (x:xs) = x

