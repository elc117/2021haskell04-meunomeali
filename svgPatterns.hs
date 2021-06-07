import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)


-------------------------------------------------------------------------------
-- Paletas
-------------------------------------------------------------------------------

-- Paleta (R, G, B) só com tons de verde "hard-coded" 
-- (pode ser melhorado substituindo os valores literais por parâmetros)
-- Além disso, o que acontecerá se n for muito grande ou negativo?
greenPalette :: Int -> [(Int,Int,Int)]
greenPalette n = [(0, 80+i*10, 0) | i <- [0..n] ]

-- Paleta com n valores retirados de uma lista com sequências de R, G e B 
-- O '$' é uma facilidade sintática que substitui parênteses
-- O cycle é uma função bacana -- procure saber mais sobre ela :-)
rgbPalette :: Int -> [(Int,Int,Int)]
rgbPalette n = take n $ cycle [(255,0,0),(0,255,0),(0,0,255)]

-- Paleta tons vermelho
genReds :: Int -> [(Int,Int,Int)]
genReds numRepet =  map (\red -> ( (if red >= 255 then 255 else red),0,0) ) $ take (numRepet*numRepet) (iterate (newRed+) firstRed)
  where newRed = 1
        firstRed = 1

-- Paleta de branco para vermelho
genWhite :: Int -> [(Int,Int,Int)]
genWhite numRepet = map (\white -> (255, (if white>=255 then 0 else (255-white)),(if white>=255 then 0 else (255-white))) ) $ take (numRepet*numRepet) (iterate (newWhite+) firstWhite)
  where newWhite = 2
        firstWhite = 0

-------------------------------------------------------------------------------
-- Geração de retângulos em suas posições
-------------------------------------------------------------------------------

-- preto para vermelho começando em (0,0), indo até (n,n)
genRectsInLine1 :: Int -> [Rect]
genRectsInLine1 n  = [((m*(w+gap), x*(w+gap)), w, h) | m <- [0..fromIntegral (n-1)], x <- [0..fromIntegral (n-1)]]
  where (w,h) = (50,50)
        gap = 10



genRectsInLine2 :: Int -> [Rect]
genRectsInLine2 n  = [((x*(w+gap), m*(w+gap)), w, h) | m <- [0..fromIntegral (n-1)], x <- [0..fromIntegral (n-1)]]
  where (w,h) = (50,50)
        gap = 10

--

-- branco para vermelho começando em (n,n), indo até (0,0)
genRectsInLine3 :: Int -> [Rect]
genRectsInLine3 n  = [((m*(w+gap), x*(w+gap)), w, h) | m <- [fromIntegral (n-1), fromIntegral (n-2)..0], x <- [fromIntegral (n-1), fromIntegral (n-2)..0]]
  where (w,h) = (50,50)
        gap = 10


genRectsInLine4 :: Int -> [Rect]
genRectsInLine4 n  = [((x*(w+gap), m*(w+gap)), w, h) | m <- [fromIntegral (n-1), fromIntegral (n-2)..0], x <- [fromIntegral (n-1), fromIntegral (n-2)..0]]
  where (w,h) = (50,50)
        gap = 10

-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensões do retângulo e uma string com atributos de estilo
svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style

-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo
-- Recebe uma função geradora de strings SVG, uma lista de círculos/retângulos e strings de estilo
svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

main :: IO ()
main = do
  writeFile "figs.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgfigs1 ++ svgfigs2 ++ svgfigs3 ++ svgfigs4 ++ svgEnd
        -- tons de vermelho
        svgfigs1 = svgElements svgRect rects1 (map svgStyle palette1)
        rects1 = genRectsInLine1 nrects
        palette1 = genReds nrects
        -- tons de vermelho
        svgfigs2 = svgElements svgRect rects2 (map svgStyle palette2)
        rects2 = genRectsInLine2 nrects
        palette2 = genReds nrects

        -- tons de branco para vermelho
        svgfigs3 = svgElements svgRect rects3 (map svgStyle palette3)
        rects3 = genRectsInLine3 nrects
        palette3 = genWhite nrects
        -- tons de branco para vermelho
        svgfigs4 = svgElements svgRect rects4 (map svgStyle palette4)
        rects4 = genRectsInLine4 nrects
        palette4 = genWhite nrects

        nrects = 12

        (w,h) = (1000,1000) -- width,height da imagem SVG



