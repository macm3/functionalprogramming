
-- Funcoes Polimorficas

tamLista :: [Int] -> Int
tamLista [] = 0
tamLista (x:xs) = 1 + tamLista xs

tamListaPoli :: [a] -> Int
tamListaPoli [] = 0
tamListaPoli (x:xs) = 1 + tamListaPoli xs

mzip :: [a] -> [b] -> [(a,b)]
mzip [] _ = []
mzip _ [] = []
mzip (x:xs) (y:ys) = [(x, y)] ++ mzip xs ys

mzip2 :: [a] -> [b] -> [(a,b)]
mzip2 (x:xs) (y:ys) = [(x, y)] ++ mzip2 xs ys
mzip2 _ _ = []

-- Funcoes de alta ordem

soma3Int :: Int -> (Int -> (Int -> Int))
soma3Int x y z = x + y + z

aplicaDuasVezes :: (t -> t) -> t -> t
aplicaDuasVezes f x = f (f x)

total :: (Int -> Int) -> Int -> Int
total f 0 = f 0
total f n = f n + total f (n-1)

h :: Int -> Int
h 0 = 5
h 1 = 6
h 2 = 9
h 3 = 15

totalH n = total h n

sq :: Int -> Int
sq x = x * x

totalSq n = total sq n

-- Sobre listas

-- mapeamento

dobrarValoresLista :: [Int] -> [Int]
dobrarValoresLista [] = []
dobrarValoresLista (x:xs)= (2 * x):dobrarValoresLista xs 
-- [2 * x | x <- l]

quadradoLista :: [Int] -> [Int]
quadradoLista [] = []
quadradoLista (x:xs)= (x * x):quadradoLista xs 
-- [2 * x | x <- l]


map1 :: (a -> b) -> [a] -> [b]
map1 f [] = []
map1 f (x:xs) = f x : map1 f xs

ehPar :: Int -> Bool
ehPar x = x `mod` 2 == 0

{-
map (map (+1)) [[1,2,3],[4,5]]
= 
[map (+1) [1,2,3], map (+1)[4,5]]
=
[[2,3,4],[5,6]]
-}

-- Filtro (filter)

filtro :: (t -> Bool) -> [t] -> [t]
filtro f [] = []
filtro f (x:xs) 
  | f x = x : filtro f xs
  | otherwise = filtro f xs

filtro2 p l = [x | x <- l, p x]

--
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

prodLista :: [Int] -> Int
prodLista [] = 1
prodLista (x:xs) = x * prodLista xs

{-
f [] = v
f (x:xs) = x # f xs
-}

mfoldr :: (a -> b -> b) -> b -> [a] -> b
mfoldr f v [] = v 
mfoldr f v (x:xs) = f x (mfoldr f v xs)

somaLista2 l = foldr (+) 0 l

tamLista :: [a] -> Int
tamLista [] = 0
tamLista (_:xs) = 1 + tamLista xs

tamLista2 :: [a] -> Int
tamLista2 = foldr (\_ n -> 1 + n) 0 