--A. Calcular El volumen de una esfera
volumenEsfera :: Float -> Float
volumenEsfera r = 4/3 * pi * r^3

--B. La suma de monedas tal que (sumaCoins a b c d e) es la suma de los pesos correspondientes a: a
-- monedas de 1 centavo, b de 5 centavos, c de 10 centavos, d de 50 centavos, e de 1 peso.
sumaCoins :: (Int, Int, Int, Int, Int) -> Float
sumaCoins (a, b, c, d, e) = fromIntegral a * 0.01 + fromIntegral b * 0.05 + fromIntegral c * 0.1  + fromIntegral d * 0.5  + fromIntegral e