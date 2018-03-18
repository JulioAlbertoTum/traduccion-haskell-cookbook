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