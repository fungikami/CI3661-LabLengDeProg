-- Ejercicio 3
-- newtype Secuencial s a = Secuencial (s -> (a, s))
-- instance Monad (Secuencial s) 
--     where
--         -- firmas para las funciones return, >>=, >> y fail 
--         (>>=) :: Secuencial s a -> (a -> Secuencial s b) -> Secuencial s b
--         (>>) :: Secuencial s a -> Secuencial s b -> Secuencial s b
--         Secuencial s >> x = Secuencial s >>= \_ -> x
--         return :: a -> Secuencial s a
--         fail :: String -> Secuencial s a

--         return a = Secuencial $ \s -> (a, s)

-- (Secuencial programa) >>= transformador = 
--     Secuencial $ \estadoInicial -> 
--     let (resultado, nuevoEstado) = programa estadoInicial
--         (Secuencial nuevoPrograma) = transformador resultado
--     in nuevoPrograma nuevoEstado

