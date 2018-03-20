## Usando Maybe

Maybe es de tipo suma usado muy a menudo para indicar un valor *NULL*  o nada. De hecho, es una forma muy segura de representar *NULL*. Explicitamente nos dice si tenemos un valor o no tenemos nada, haciendo nuestra vida mas facil con los pasos de programacion que pueden fallar y que no podamos continuar.

En esta receta, aprenderemos el tipo de dato **MayBe**

---

### Para empezar 

Creamos un nuevo proyecto llamado **using-maybe** con el template *simple*
```bash
stack new using-maybe simple
stack build
```

### Como hacemos... 

1. Abrimos *src/Main.hs*. Reemplazamos la funcion main con el siguiente contenido:
```hs
main :: IO ()
main = do
  putStrLn "Using maybe"
```
2. Continua en la misma funcion. Comienza definiendo varios valores de Maybe. Maybe es de tipo suma que puede contener un valor. el constructor de datos es **Just** que toma el valor, mientras que el constructor **Nothing** representa la  ausencia de valor. Definimos 3 instancias del Valor Maybe, reprensentando valores 10, 2, y 0:
```hs
let i = Just 10 :: Maybe Int
      j = Just 2 :: Maybe Int
      z = Just 0 :: Maybe Int
```
3. Nota la indentacion. Como esto es parte de la misma funcion *main*. la indentacion deberia estar a la misma altura que *putStrLn "Using Maybe" *.
4. Usamos la funcion **isJust** para verificar si el valor *Maybe* contiene algun valor. Usamos **isNothing** para verificar la ausencia de valor.
```hs
  putStrLn $ " (Just 10) representa un valor? " ++ (show isJust i)
  putStrLn $ " (Nothing) representa un valor? " ++ (show isJust Nothing)
  putStrLn $ " (Nothing) es realmente Nada? " ++ (show isNothing Nothing)
```
5. Una singleton list es una lista que contiene un solo elemento. Las funciones **listToMaybe** y **maybeToList** convierte un singleton list a un *Maybe*  y *Maybe* a una singleton list, respectivamente. La lista vacia se corresponde con *Nothing* , mientras que una lista con un elemento pondra el valor del elemento en *Maybe*:
```hs
  putStrLn ""
  putStrLn $ "Singleton list y Maybe interoperabilidad"
  putStrLn $ "Convierte list [10] a Maybe: " ++ (show $ listToMaybe [10])
  putStrLn $ "Convierte una lista vacia a Maybe (Nothing) : " ++ (show $ (listToMaybe [] :: Maybe Int))
  putStrLn $ "Convierte Maybe (Just 10) a list: " ++ (show maybeToList (Just 10))
  putStrLn $ "Convierte Maybe (Nothing) to list: " ++ (show maybeToList (Nothing :: Maybe Int))
```
6.  Usa la funcion *maybe :: b -> (a -> b) -> Maybe a -> b*. La funcion *Maybe* toma un valor por defecto *b* y una funcion *(a -> b)*. Si el valor de *Maybe* es *Nothing*, se usa el valor por defecto *b*.  Caso contrario  este usara la funcion  (a -> b)  para transformar el valor de a de *Maybe* en *b*:
```hs
putStrLn ""
  putStrLn "Usando el valor por defecto para una transformacion usando 'maybe'"
  putStrLn $ "Usa NULL si Nothing, y convierte un valor a string si Maybe guarda un valor"
  let defaultNull = "NULL"
      convertToString = maybe defaultNull show
      null = convertToString Nothing
      something = convertToString (Just 10)
  putStrLn $ "Convirtiendo Nothing a String : " ++ null
  putStrLn $ "Convirtiendo un valor a String : " ++ something
```
7. Aqui, usaremos el valor por defecto *NULL* y convertir un valor *Maybe* a String. Si *Maybe* contiene algun valor, este se convertira a una representacion en string usando show. En  otro caso NULL es impreso.
8. Ahora usaremos *Maybe* en un ejemplo que ilustre su uso. Definimos la funcion *safeOperation*, que define una operacion binaria segura. Si una cierta condicion se satisface, entonces evaluaremos el resultado como *Nothing*. Usaremos esto para representar una division entre cero en la funcion **safeDiv**
```hs
  safeOperation :: Num a => (a -> a -> Bool) -> ( a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
  -- Si cualquier de las entradas es Nothing, entonces la salida sera  Nothing 
  safeOperation _ _ Nothing _ = Nothing
  safeOperation _ _ _ Nothing = Nothing
  -- si la operacion se encuentra, entonces el resultado es nothing
  safeOperation c _ (Just i) (Just j) | c i j = Nothing
  -- Llamada normal a la operacion
  safeOperation c op (Just i) (Just j) = Just(i `op` j)

  -- safe division, la condicion es satisfecha cuando el denominador es diferente de 0
  safeDiv :: Maybe Int -> Maybe Int -> Maybe Int
  safeDiv = safeOperation divCondition div
    where
      divCondition _ 0 = True
      divCondition _ _ = False
```
9. Usa la funcion *safeDiv* en la funcion *main* para verificar el resultado de hacer division entre cero.Con la siguiente division ilustramos el caso normal y el caso con division no valida (division entre cero). Cuando dividimos entre cero deberiamos obtener *Nothing*
```hs
putStrLn ""
  putStrLn $ "Obtenemos el valor de (Just 10) = " ++ (show $ fromJust i)
  putStrLn $ "Division segura - 10 / 2"
  let safeAnswer1 = safeDiv i j
  putStrLn $ "Respuesta es" ++ (show safeAnswer1)
  putStrLn ""
  putStrLn $ "Division segura por Cero - 10 / 0"
  let safeAnswer2 = safeDiv i z
  putStrLn $ "La respuesta es " ++ (show safeAnswer2)
```
10. Tambien podemos definir la division segura usando la notacion monadica *do*. Define la funcion *safeDiv1* en el archivo *src/Main.hs*
```hs
  safeDiv1 :: Maybe Int -> Maybe Int -> Maybe Int
  safeDiv1 i j = do
    xi <- i
    xj <- j
    if 0 == xj
      then
      Nothing
      else
      return (xi `div` =xj)
```
11. Note el uso de *return* y *(<-). Usar *safeDiv1* en la funcion *main*. Adiciona las siguientes lineas a la funcion *main*
```hs
  putStrLn ""
  putStrLn $ "Podemos usar la notacion - do"
  let safeAnswer3 = safeDiv1 i z
  putStrLn $ "Division segura entre cero usando la notacion - 10 / 0 " ++ (show safeAnswer3)
```
12. Finalmente, usamos *mapMaybe*. Esta funcion es simillar a map, expecto que toma una funcion que produce los valores de Maybe. Todos los valores *Nothing* son filtrados, y el resto de valores extraidos de la lista:
```hs
putStrLn ""
  let evens = mapMaybe (\x -> if odd x then Nothing else (Just x)) [1..10]
  putStrLn $ "Filtrando elementos impares - mapMaybe (\\a -> Si impar entonces Nothing else (Just a)) [1..10]" ++ (show evens)
``` 
13. En los ejemplos precedentes, filtramos los numeros impares. Para cada numero impar producimos *Nothing*; en otro caso solo imprimimos *Just x* (donde x es el valor de entrada).
14. Construimos y ejecutamos:
```bash
stack build 
stack exec -- using-maybe
```
---
### Como fuciona...

El tipo de dato **Maybe** esta definido como sigue.
```hs
data Maybe a = Just a
  | Nothing
```
Es de tipo suma, que o representa un valor, usando el constructor *Just*, o *Nothing*. Varias funciones tales como *isJust*, *isNothing*, *fromJust*, *listToMaybe0* y *maybeToList* son usadas para verificar o extraer los valores de Maybe.
La funcion mas interesante es *SafeOperation*. Este ilustra el uso de Maybe. El valor *Nothing* denota una falla de algun tipo en una operacion. Como resultado, el resto de operacion deberian producir *Nothing*. Esto nos ahorra el esfuerzo de verificar el resultado de una operacion todo en un paso. La signatura de una operacion segura es:
```hs
safeOperation :: Num a => (a -> a -> Bool) -> (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
``` 
El primer argumento (a -> a -> Bool) denota una condicion. Si la condicion es evaluada a *True*, entonces el resultado producido es *Nothing*. El segundo argumento *(a -> a -> a)* denota una operacion que trabaja sobre 2 entradas de tipo a y produce una salida de tipo a. El tercer y cuarto argumento son de tipo Maybe. Estos son los argumentos pasados a la funcion *safeOperation*. Si cualquiera de estos argumentos es igual  *Nothing*, el resultado es Nothing. En otro caso, si la condicion no es *True*, el segundo argumento *a -> a -> a* es usado para llevar a cabo la operacion subyacente.
Usando esta funcion, podemos convertir una operacion binaria tal como la division, y convertir esta en una operacion segura. En cualquier punto si la condicion es conocida o si cualquiera de las entradas es *Nothing*, todas las operaciones produciran nada.
Podemos tambien usar notacion *monadica* para representar la division segura. Esto es interesante. La notacion *do* nos permite extraer valores de Maybe usando *(<-)*, y entonces usamos return para representar *Just*. Sin embargo, reservaremos esta explicacion para el siguiente capitulo donde discutiremos el uso de esta notacion en detalle.  