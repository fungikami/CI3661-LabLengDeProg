-- Ejercicio 1
type Conjunto a = a -> Bool

-- Debe devolver la pertenencia en el conjunto proporcionado, de un elemento dado.
miembro :: Conjunto a -> a -> Bool
-- miembro conjunto x = conjunto x
miembro = id

-- Debe devolver un conjunto vacío
vacio :: Conjunto a
-- vacio x = False
vacio = const False

-- Debe devolver un conjunto que contenga únicamente al elemento proporcionado.
singleton :: (Eq a) => a -> Conjunto a
-- singleton x y = x == y
singleton = (==)

-- Debe devolver un conjunto que contenga a todos los elementos de la lista 
-- proporcionada
desdeLista :: (Eq a) => [a] -> Conjunto a
-- desdeLista xs x = elem x xs
desdeLista = flip elem

-- Debe devolver un conjunto que contenga únicamente todos los elementos que no 
-- estén en el conjunto proporcionado (pero que sean del mismo tipo).
complemento :: Conjunto a -> Conjunto a
-- complemento conjunto x = not (conjunto x)
complemento = (not .)

-- Debe devolver un conjunto que contenga todos los elementos de cada conjunto
-- proporcionado.
union :: Conjunto a -> Conjunto a -> Conjunto a
union conj1 conj2 x = conj1 x || conj2 x

-- Debe devolver un conjunto que contenga solo los elementos que estén en los dos 
-- conjuntos proporcionados.
interseccion :: Conjunto a -> Conjunto a -> Conjunto a
interseccion conj1 conj2 x = conj1 x && conj2 x

-- Debe devolver un conjunto que contenga los elementos del primer conjunto 
-- proporcionado, que no estén en el segundo.
diferencia :: Conjunto a -> Conjunto a -> Conjunto a
-- diferencia conj1 conj2 x = conj1 x && not (conj2 x)
diferencia conj1 conj2 = interseccion conj1 (complemento conj2)

-- Dada una función f y un conjunto A, devuelva un conjunto B tal que A es el 
-- resultado de aplicar f a todos los elementos de B. Por ejemplo, si la función f 
-- es sumar 1 y el conjunto A es {2, 4, 8}, un conjunto resultado podrı́ a ser 
-- {1, 3, 7} (nótese que tal conjunto podrı́a no ser único, por lo que se espera que 
-- retorne cualquiera de ellos, en caso de existir varias opciones)
transformar :: (b -> a) -> Conjunto a -> Conjunto b
transformar f a x = a (f x)

-- Examples
main = do
    -- Miembro
    print $ miembro ( == 1 ) 1 -- True
    print $ miembro ( == 2 ) 1 -- False

    -- Vacio
    print $ vacio 1 -- False

    -- Singleton
    print $ singleton 1 1 -- True
    print $ singleton 1 2 -- False

    -- DesdeLista
    print $ desdeLista [1, 2, 3] 1 -- True

    -- Complemento
    print $ complemento ( == 1 ) 1 -- False

    -- Union
    print $ union ( == 1 ) ( == 2 ) 1 -- True

    -- Interseccion
    print $ interseccion ( == 1 ) ( == 1 ) 1 -- True

    -- Diferencia
    print $ diferencia ( == 1 ) ( == 2 ) 1 -- True

    -- Transformar
    print $ transformar (+1) ( == 1 ) 1 -- True

