## Trabajando con Functores
En esta receta usaremos la Clase de tipo **Functor** para hacer algunas tareas mas facil. Veremos como un *Functor* se asemeja a *map* de una lista de forma que se aplique a una variedad de estructuras de datos.

### Como hacemos...

1. Usamos stack para crear un nuevo proyecto working-functors con el template simple:
```hs
stack new working-with-functors simple
```
2. Abrimos *src/Main.hs* en el editor. Usaremos este archivo para demostrar el uso de *Functors*.
3. Despues en el archivo *Main.hs*, importaremos el modulo que incluye la clase de tipo *Functor*
```hs
import Data.Functor
```
4. Definimos una funcion para elevar al cuadrado un numero. Usaremos esto para demostrar la aplicacion de esta funcion sobre varias estructuras de datos.
```hs
-- Elevar al cuadrado un numero
square :: Num a => a -> a
square x = x * x
```
5. Functor f es una clase de tipo que necesita *fmap :: (a -> b) -> f a -> f b*. Data.Functor define una funcion <$>  sinonimo de *fmap*. List define una instancia para el Functor. Usaremos la funcion *square*  para aplicarlo sobre la lista. Adicionamos el siguiente codigo para obtener el cuadrado de todos los elementos de la lista.
```hs
--Mapeando una lista
squareList :: Num a => [a] -> [a]
squareList xs = square <$> xs
```
6. Similarmente, podemos usar <$> para aplicar sobre  los tipos de dato *Maybe* y *Either*. *Maybe* permite a una funcion ser aplicada si el dato es representado como Just. En otro caso la funcion no es aplicada. La instancia Either para *functor* permite a la funcion ser aplicada solo cuando el constructor *Right* es usado:
```hs
--Mapeando un Maybe
squareMaybe :: Num a => Maybe a -> Maybe a
squareMaybe x = square <$> x

--Mapeando un Either
squareEither :: Num a => Either c a -> Either c a
squareEither x = square <$> x
```
7. Ahora, definimos un tipo de dato *Function a b* para representar una funcion *a -> b*. Definiremos esta para ser una instancia de Functor. La instancia Functor para este tipo de dato creara una composicion usando el operador de composicion de funciones *(.)*:
```hs
data Function a b = Function (a -> b)
instance Functor (Function a) where
	f `fmap` (Function g) = Function (f . g)
```
8. Definimos otra funcion util *double* para hallar el doble de un valor. Usaremos esta en *main* para demostrar la composicion de funciones:
```hs
double :: Num a => a -> a
double x = x + x
``` 
9. Ahora, adicionamos la funcion *main* donde pondremos a prueba las definiciones:
```hs
main :: IO ()
main = do
  putStrLn "Mapeando una lista"
  putStrLn $ show $ squareList [1..20]

  putStrLn ""
  putStrLn "Mapeando Maybe"
  putStrLn "Justo 10 -> Just 100"
  putStrLn $ show $ squareMaybe (Just 10)

  putStrLn ""
  putStrLn "Nothing -> Nothing"
  putStrLn $ show $ squareMaybe Nothing

  putStrLn ""
  putStrLn "Mapeando Either"
  putStrLn "Right 10 -> Right 100"
  putStrLn $ show $ squareEither (Right 10 :: Either String Int)
  putStrLn "Left String -> Left String"
  putStrLn $ show $ squareEither (Left "Left Value" :: Either String Int)

  let squareF = Function square
      doubleSquare = double <$> squareF

  --Toma el resultado de la funcion doubleSquare
  let Function dsq = doubleSquare
  putStrLn "El doble del cuadrado de X"
  print $ dsq 10
```
## Como funciona...
El modulo *Data.Functor* define la clase de tipo Functor. Esta clase esta disponible tambien en el preludio, pero definido en *Data.Functor*. La clase de tipo Functor es definida como sigue:
```hs
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
```
La definicion minima de *Functor* requiere *fmap* para ser definido.

La funcion *fmap* toma una funcion *a -> b* y toma una tipo de dato f parametrizado por a. Este aplica la funcion  al contenido (de tipo a) para producir b, asi producimos *f b*.

Por ejemplo, podemos tomar *Maybe a*, que es definido como *Just a | Nothing*. Cuando aplicamos una funcion *a -> b* a *Maybe a* a traves de *fmap*, aplicandose solo al constructor *Just a* produciendo *Just b*. El constructor de datos *Nothing* no necesita ser operado.

Definicion de *Functor* para *Maybe* y *Either* es definido en el siguiente diagrama: 
- *Imagen No Disponible*

Note como *fmap*  es definido para *Either c a*. La funcion *a -> b* es aplicado solo al constructor *Right a*, el constructor *Left c* es pasado normalmente sin alteracion. En el caso de *List [a], fmap* es lo mismo que la funcion *map*, esto es, la funcion es aplicada a cada elemento de list.

Asi, cuando aplicamos  la funcion *square*  sobre los tipos de dato *Maybe* y *Either*, la funcion *square* se aplicara solo cuando los tipos de datos son definidos con los constructures de datos *Just* y *Right*, respectivamente.

De forma similar, hacemos la definicion de functor para nuestro tipo de dato *Function a b*. La definicion *fmap* toma una funcion y compone esta con la funcion apuntada por el constructor de datos *Function (a -> b)*.

La clase de tipos *Functor* tambien define la funcion (<$). Este toma un valor de tipo *a* y simplemente reemplaza *b* en *f b*. Ninguna definicion es requerida por esta funcion asi puede ser definida usando la funcion const:
```hs
(<$) :: a -> f b -> f a
(<$) a fb = fmap (const a) fb
```  