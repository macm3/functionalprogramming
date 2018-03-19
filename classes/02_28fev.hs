
--funcaoConstante :: Int
funcaoConstante = 30

compara30 :: Int -> Bool
compara30 x = x > funcaoConstante

comparaMultiplica :: Int -> Int
comparaMultiplica x =
 if x < 10
 then x 
 else x * 5

comparaMultiplica2 :: Int -> Int
comparaMultiplica2 x 
  | x < 10 = x
  | otherwise = (x * 5)

mAnd :: Bool -> Bool -> Bool
mAnd True b = b
mAnd _ _ = False

mFst :: (Int,Int) -> Int
mFst (x,y) = x

fatorial :: Int -> Int
fatorial 1 = 1
fatorial n = n * fatorial (n-1)


add3Int :: Int -> Int -> Int -> Int
add3Int x y z = x + y + z

add3Int_2 = add3Int 2

add2Int_Lambda :: Int -> Int -> Int
add2Int_Lambda x = h
  where
    h = (\y -> x + y)

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

mReverse :: [Int] -> [Int]
mReverse [] = []
mReverse (x:xs) = mReverse xs ++ [x]

mDrop :: Int -> [Int] -> [Int]
mDrop n [] = []
mDrop 0 l = l
mDrop n (x:xs) = mDrop (n-1) xs 

dobrarLista :: [Int] -> [Int]
dobrarLista l = [ 2* x | x <- l]

dobrarListaPares :: [Int] -> [Int]
dobrarListaPares l = [ 2* x | x <- l, x `mod` 2 == 0]