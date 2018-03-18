--para compilar:
--1) ir para a pasta do arquivo 
--2) ghci no terminal
--3) :load nome_arquivo.hs
--4) input

fatorial :: Int -> Int
fatorial 1 = 1
fatorial n = n * fatorial (n-1)

mDrop :: Int -> [Int] -> [Int]
mDrop n [] = []
mDrop 0 l = l
mDrop n (x:xs) = mDrop (n-1) xs