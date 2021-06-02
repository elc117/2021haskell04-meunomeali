-- Prática 04 de Haskell
-- Nome: Álisson Braga Canabarro

-- 1) Retorna uma sigla dependendo da idade selecionada
faixaIdoso :: Int -> String
faixaIdoso int = if int < 60 then "ND"
else if int < 65 then "IDO64"
else if int < 70 then "IDO69"
else if int < 75 then "IDO74"
else if int < 80 then "IDO79"
else "IDO80"


-- 2) Recebe uma lista de tuplas com nome e idade, retornando uma lista de tuplas com nome idade e uma sigla de acordo com a idade
classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos lTupla = [(str,int,faixa) | (str,int) <- lTupla, let faixa = (faixaIdoso int)]

-- 3) Mesmo que anterior, sem list comprehension
classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' lTupla = map (\(str,int) -> (str,int,(faixaIdoso int))) lTupla


-- 4) Recebe uma tupla com 3 valores e retorna uma string no formato rgb com esses 3 valores
strColor :: (Int,Int,Int) -> String
strColor tupla = (\(r,g,b) -> "rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")") tupla


-- 5) Circulos de mesmo tamanho, mesma coord y e coord x aumentando de 4 em 4
genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int, Int)]
genCircs numCirc cordInicial raio = map (\x -> (x, ((\(_,cy) -> cy) cordInicial) ,raio)) $ (\(cx,cy) -> take numCirc (iterate (4+) cx)) cordInicial


-- 6) 
genReds :: Int -> [(Int,Int,Int)]
genReds numRepet =  map (\red -> (red,0,0) ) $ take numRepet (iterate (10+) 40)