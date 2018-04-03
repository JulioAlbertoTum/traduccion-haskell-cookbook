## Trabajando con Aplicativos
Un *aplicativo* es una clase de tipos que esta entre un **Functor** y una **monada**. Un aplicativos lleva a un *functor* un paso mas alla. Un functor lleva una aplicacion de una funcion *a -> b* a un tipode dato *f a*, mientras que un *Aplicativo* trara sobre la aplicacion de un tipo de dato de una funcion *f (a -> b)* a un tipo de dato *f a*.

En esta receta, trabajaremos con  los tipos de datos *Maybe* y *Either*, y veremos como podemos trabajar con instancias de *Aplicativos* en el contexto de estos tipos de datos.

### Como lo hacemos...

1. Creamos un proyecto nuevo *working-applicative* con stack y la plantilla simple:
```bash
stack new working-applicative simple
```
2. Abrimos *src/Main.hs* y adiciona los siguientes imports despues de la inicializacion del modulo. La clase de tipos *Applicative* es definnido en el modulo *Control.Applicative*.
```hs
import Data.Functor
import Control.Applicative
```
3. Nosotros usaremos 2 operadores, Aplicacion de *Functor* <$> (sinonimo para fmap) y aplicacion de *Applicative* <\*> es definido como *<\*> :: f (a -> b) -> f a -> f b*. Este toma un tipo de datos donde los valores son funciones de tipo (a -> b). y se aplica a un tipo de datos con valores de tipo *a* y obtiene el tipo de dato con valores de tipo *b*. En este primer ejemplo, usaremos una lista:
```hs
-- Mapear una lista
multiplyLists :: Num a => [a] -> [a] -> [a]
multiplyLists xs ys = (*) <$> xs <*> ys
``` 
En la siguiente aplicacion de *Applicative*, usaremos *Maybe*.
```hs
tupleMaybe :: Maybe a -> Maybe b -> Maybe (a,b)
tupleMaybe x y = (,) <$> x <*> y
```
Usaremos *Either* en el contexto de *Applicative*. Aqui, en lugar de usar *Functor*, usaremos *pure* para convertir una funcion en una *Applicative*:
```hs
-- Mapear un Either
addEither :: Num a => Either c a -> Either c a -> Either c a
addEither x y = pure (+) <*> x <*> y
```
4. Usamos las funciones precedentes en *main* con ejemplos simples:
```hs
main :: IO ()
main = do
  putStrLn "multiplicar listas"
  putStrLn $ show $ multiplyLists [1..3] [11..13]
  putStrLn ""
  putStrLn "Tuplas de maybes"
  putStrLn "Just 10 -> Just \"String\" -> Just (10, \"String\")"
  putStrLn $ show $ tupleMaybe (pure 10) (Just "String")
  putStrLn ""
  putStrLn "Just 10 -> Nothing -> Nothing"
  putStrLn $ (show :: Maybe (Int, String) -> String) $ tupleMaybe (Just 10) Nothing
  putStrLn ""
  putStrLn "Adicionando Either"
  putStrLn "Right 10 -> Right 10 -> Right 110"
  putStrLn $ (show :: Either String Int -> String) $ addEither (Right 10) (Right 100)

  putStrLn "Left String -> Right 10 -> Left String"
  putStrLn $ (show :: Either String Int -> String) $ addEither (Left "String") (Right 10)
```
### Como funciona 
La clase de tipo *Applicative* es definido como sigue:
```hs
class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  (*>)  :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  {-# Minimal pure, (<*>) #-}
```
La definicion minima de una instancia de *Aplicativo* requiere que al menos *pure* y <\*> esten definidas. La definicion tambien implica que podemos definir una instancia de un *Applicative* para f solo si f es tambien una instancia de un *Functor*.

La funcion *pure* toma un valor y crea un tipo de dato. Por ejemplo, en el contexto de *List,Maybe*, y *Either*, la funcion *pure* fijara los siguientes valores:
```hs
pure 10 :: [Int] [10] -- en el contexto de lista
pure 10 :: Maybe Int = Just 10 -- en el contexto de Maybe
pure 10 :: Either String Int = Right 10 -- en el contexto de Either
``` 
Puedes tratar el codigo precedente en la consola GHCi para el proyecto para ejecutar con *stack ghci* en el directorio del proyecto y mostrara las precedentes expresiones.

Como vimos el corazon de un *Applicative*, esto es, la funcion <*>. Como explicamos antes, esta funcion tiene la forma siguiente:
```hs
<*> :: Applicative f => f (a -> b) -> f a -> f b
```
Recuerda la definicion de un functor y fmap:
```hs
fmap :: (a -> b) f a -> f b
```
Si tomamos una funcion *(a -> b -> c)* y llamamos fmap sobre una instancia de Functor, obtendremos el siguiente codigo.
```hs
fmap :: (a -> b -> c) -> f a -> f (b -> c)
```
Esto resulta interesante porque la aplicacion de fmap resulta en *f (b -> c)*. Ahora si tomamos *f ( b -> c)* y aplicamos este para obtener *f b* usando <*> y obtenemos *f c*. Asi podemos usar una funcion tal como (*) :: a -> a -> a y usamos este en la conjuncion de <$> y <*> para aplicar cosas mas complejas como multiplicaccion sobre uan cupla de *Maybes*s:
```hs
(*) <$> Just 10 <*> Just 2 -- producira Just 20
(*) <$> Nothing <*> Just 2 -- producira Nothing
```
De esta forma, uno puede ver que *Applicative* extiende *Functor* para adicionar mas expresividad a este.
Un *Applicative* hace mucho mas que solo aplicar una funcion con multiples argumentos para un tipo de dato. En el *Applicative*, encapsularemos la funcion en el tipo de dato *f (a -> b)* y aplicar este a los tipos de datos con *f a*. De esta forma, es posible llevar mas informacion en la estructura f y aplicar este durante la evaluacion y aplicacion de la funcion encapsulada.
Por ejemplo, uno puede considerar *f a* como un operacion llevada a cabo en paralelo; *f (a -> b) denota que necesita esperar por el valor producido por *f a* y entonces aplicar la funcion. Mas aun, podemos crear tipo *Applicative* que representa un grupo de hilos y planifica *f a* sobre cada uno de ellos, reteniendo el poder de la composicion de funciones.

