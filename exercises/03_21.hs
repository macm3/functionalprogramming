--Exercicios do slide

--Questao 1
intNaLista :: Int -> [Int] -> Bool
intNaLista n [] = False
intNaLista n (x:xs) = 
            if n == x then True
            else intNaLista n xs
