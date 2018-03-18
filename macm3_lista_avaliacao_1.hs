--Questão 1 - Criptografia

import Data.Char

--Codificando
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n cs = [shift n c | c <- cs]

--Decodificando
percent :: Int -> Int -> Float
percent m n = ((fromIntegral m / fromIntegral n) * 100)

freqs :: String -> [Float]
freqs cs = [percent (count c cs) n | c <- ['a'..'z']]
    where n = lowers cs
          lowers cs = length [c | c <- cs, isLower c]
          count c cs = length [c' | c' <- cs, c == c']
--freqRel = [14.63, 1.04,3.88,4.99,12.57,1.02,1.3,1.28,6.18,0.4,0.02,2.78,4.74,5.05,10.73,2.52,1.2,6.53,7.81,4.34,4.63,1.67,0.01,0.21,0.01,0.47]

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]
          
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions c cs = [i | (c', i) <- zip cs [0..n], c == c']
    where n = length cs - 1

crack :: String -> String
crack cs = encode (-factor') cs
           where factor' = head $ positions (minimum chitable) chitable
                 chitable = [chisqr (rotate n table') table | n <- ns]
                 table' = freqs cs
                 ns = [0..(26 - 1)]

--Questao 2 - Tautologia

import Data.List (nub)

data Prop = Var Char 
            | Const Bool
            | Neg Prop
            | Disj Prop Prop
            | Conj Prop Prop
            | Impl Prop Prop
            deriving (Eq, Show)