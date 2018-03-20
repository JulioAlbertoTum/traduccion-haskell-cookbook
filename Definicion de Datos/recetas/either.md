## Usando Either

Similar a *Maybe*, otro tipo de dato muy usado en haskell es Either. Mientras que *Maybe* decide si mapea algo no nada, *Either* ira con 2 tipos y guardara uno de ellos. En esta receta, construiremos una division segura usando el tipo de dato *Either*  y veremos como podemos representar el mensaje de error de una mejor forma.

### Comenzamos
Usamos el siguiente comando para crear un nuevo proyecto *using-either* con el template *simple*:
```bash
stack new using-either simple
```
### Como seguimos
1. Abrimos *src/Main.hs*
2. Importamos el Modulo *Data.Either*:
```hs
import Data.Either
``` 
3. Definimos la division segura, manejando el caso de division entre cero:
```hs
safeDiv :: Either String Int -> Either String Int -> Either String Int -> Either String Int
safeDiv (Left e) _ = Left e
safeDiv _ (Left e) = Left e
safeDiv (Right i) (Right j) | j == 0 = Left "Operacion Ilegal: Division entre cero"
safeDiv (Right i) (Right j) = Right (i `div` j)
```
4. Usamos la division segura en *main* para ilustrar el uso de *Either*:
```hs
main :: IO ()
main = do
  let i = Right 10 :: Either String Int
      j = Right 2 :: Either String Int
      z = Right 0 :: Either String Int
  putStrLn $ "Division segura : 10 / 2 = " ++ (show $ safeDiv i j)
  putStrLn $ "Division segura : 10 / 0  = " ++ (show $ safeDiv i z)
```
5. Construir y ejecutar el proyecto.
```bash
stack build 
stack exec -- using-either
```
---
### Como funciona...
El tipo de Dato *Either* tiene dos constructores y esta definido como un tipo suma:
```hs
data Either a b = Left a | Right b
```
En nuestra receta, usamos *Either String Int*, donde el valor right es de tipo *Int* y el valor left es de tipo *String*. En muchos ejemplos practicos el valor *left* es usado para un valor de error y el valor *Right* es usado como un resultado deseado.
En la funcion *safeDiv*, almacenamos el valor de error en *Left* como un *String* y almacenamos el resultado en el valor *Right*.
Como *Maybe*, *Either* aparece un muchas librerias o paquetes y es  una eleccion muy popular para representar valores con resultados y errores.