-- Ejercicio 2

-- Tipo de datos ArbolMB
data ArbolMB a = Vacio 
    | RamaM a (ArbolMB a)
    | RamaB a (ArbolMB a) (ArbolMB a)

-- Tipos de Vacio y RamaB, como funciones
-- Vacio :: ArbolMB a
-- RamaB :: a -> ArbolMB a -> ArbolMB a -> ArbolMB a

-- Tipos de transformarVacio, transformarRamaM y transformarRamaB, como funciones
-- transformarVacio :: b
-- transformarRamaB :: a -> b -> b -> b

-- e)
plegarArbolMB :: (b) -- El tipo de transformarVacio 
    -> (a -> b -> b) -- El tipo de transformarRamaM. 
    -> (a -> b -> b -> b) -- El tipo de transformarRamaB. 
    -> ArbolMB a -- El arbol a plegar. 
    -> b -- El resultado del plegado. 

plegarArbolMB transVacio transRamaM transRamaB = plegar 
    where 
        plegar Vacio = transVacio
        plegar (RamaM x y) = transRamaM x (plegar y) 
        plegar (RamaB x y z) = transRamaB x (plegar y) (plegar z)


-- d)
sumarArbolMB :: (Num a) => ArbolMB a -> a 

sumarArbolMB = plegarArbolMB transVacio transRamaM transRamaB
    where
        transVacio = 0
        transRamaM = \x y -> x + y
        transRamaB = \x y z -> x + y + z

-- e)
aplanarArbolMB :: ArbolMB a -> [a] 
aplanarArbolMB = plegarArbolMB transVacio transRamaM transRamaB 
    where 
        transVacio = []                     
        transRamaM = \x y -> x : y          
        transRamaB = \x y z -> x : y ++ z   

-- d)
analizarArbolMB = plegarArbolMB transVacio transRamaM transRamaB
    where
        transVacio = Nothing
        transRamaM = \x y -> case y of 
            Nothing -> Just (x, x, True) 
            Just (ymin, ymax, isSorted) -> Just (min x ymin, max x ymax, isSorted && x <= ymin)
        transRamaB = \x y z -> case (y, z) of
            (Nothing, Nothing) -> Just (x, x, True)
            (Nothing, Just (zmin, zmax, isSorted)) -> Just (min x zmin, max x zmax, isSorted && x <= zmin)
            (Just (ymin, ymax, isSorted), Nothing) -> Just (min x ymin, max x ymax, isSorted && x <= ymin)
            (Just (ymin, ymax, yisSorted), Just (zmin, zmax, zisSorted)) -> Just (min x (min ymin zmin), max x (max ymax zmax), yisSorted && zisSorted && x <= ymin && x <= zmin)



main = do
    print "Hi"
