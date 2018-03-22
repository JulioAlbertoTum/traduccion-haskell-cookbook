## Trabajando con Clases de tipos

En esta receta, aprenderas sobre las clases de tipos. Estos proveen un tipo comun de comportamiento para varios tipos de datos. De esta forma, una clase de tipos abstrae el comportamiento comun y puede ser implementado para una gran cantidad de datos. Uno puede relacionar las clases de tipos con **interfaces** de lenguajes como C# y C++.

Hasta aqui, hemos definido tipos de datos derivandolos de la clase de tipos **Show** sin dar una implementacion explicita para este tipo. En tales casos, la implementacion por defecto es provista por GHC. En esta receta daremos una implementacion explicita para las clases de tipos *Show, Read, Enum, Eq, y Ord*.

### Comenzamos
Creamos un nuevo proyecto llamado *working-type-classes* usando la plantilla simple y cambiamos el directorio
```bash
stack new working-type-classes simple
```
## Como hacemos...
1. Abrimos *src/Main.hs* para editar
2. Definimos el tipo de datos Month para describir los meses del anio.
```hs

data Month = January | February | March | April | May | Juny | July | August | September | October | November | December deriving Show
-- note que todavia hemos derivado Show. Despues veremos su funcionamiento en la receta.
```
3. A continuacion implementaremos la clase *Enum*. Esta clase es responsable para la generacion de listas de enteros consecutivos y expresiones tales como [1..10]. La clase Enum provee este comportamiento por asociacion con enteros. Creamos una instancia de la clase Enum para el tipo de datos *Month*. En esencia, necesitamos implementar 2 funciones *toEnum* y *fromEnum* para convertir  a partir de un entero a *Intergers*.

```hs
instance Enum Month where
    toEnum 0 = January
    toEnum 1 = February
    toEnum 2 = March
    toEnum 3 = Abril
    toEnum 4 = May
    toEnum 5 = June
    toEnum 6 = July
    toEnum 7 = August
    toEnum 8 = September
    toEnum 9 = October
    toEnum 10 = November
    toEnum 11 = December
    toEnum n = toEnum $ n `rem` 12

fromEnum January = 0
fromEnum February = 1
fromEnum March = 2 
fromEnum Abril = 3
fromEnum May = 4
fromEnum June = 5
fromEnum July = 6
fromEnum August = 7
fromEnum September = 8
fromEnum October = 9
fromEnum November = 10 
fromEnum December = 11
```
4. Implementamos la clase de tipo *equality* para nuestro tipo de datos Month. Esto nos da la habilidad de verificar la cantidad de valores de nuestro tipo de datos *Month*. Definimos la funcion (==). Usaremos la definicion previa de *Enum* para convertir los valores de Integer y entonces comparar estos.
```hs
instance Eq Month where
    m1 == m2 = fromEnum m1 == fromEnum m2
```
5. Ahora, implementamos la clase de tipos *Ord*. Esta representa el orden, y da el ordenamiento a los valores del tipo de datos *Month*. Ahora necesitamos definir la funcion *compare* y retornar los valores del tipo de datos *Ordering*. De nuevo usamos el hecho que hemos implementado la clase de tipo *Enum* y los *Integers* actualmente implementan la clase de tipos *Ord*. Ahora, convertimos los valores de *Month* e invocamos al metodo *compare*:
```hs
instance Ord Month where
    m1 `compare` m2 = fromEnum m1 `compare` fromEnum m2
-- note que hemos implementado la funcion `compare` usando la notacion infija
```
6. Hasta ahora hemos implementado y tipo de datos (en esta, y anteriores recetas) que hace uso de la auto implementacion de Show que nos da el compilador, GHC. Ahora, implementaremos un tipo de datos donde damos una implementacion explicita para *Show y Read*. Implementamos un tipo de datos, *RoseTree* que es un arbol n-ario.
```hs
data RoseTree a = RoseTree a [RoseTree a]
```
7. Ahora, implementamos la clase de tipos *Show*. Para *Show* tenemos que implementar una funcion *show :: a -> String*:
```hs
toString :: Show a => RoseTree a -> String -> String
toString (RoseTree a branches) = 
    ( "<<" ++) . shows a . ('[':) . branchesToString branches . (']':) . (">>" ++)
    where 
        branchesToString [] r = r
        branchesToString (x:[]) r = branchesToString [] (toString x "" ++ r)
        branchesToString (x:xs) r = branchesToString xs (',' : toString x "" ++ r)

-- Usamos la funcion precedente para implementar Show
instance Show a => Show (RoseTree a) where
    show tree = toString tree ""
```
8. Ahora, implementamos la clase de tipos *Read*. La clase Read hace el inverso de la clase *Show*; Este lee el valor String retornado por Show y convierte este a un valor del tipo. Aqui, implementaremos una instancia de clase de tipo para *RoseTree*:
```hs
instance Read a => Read (RoseTree a) where
    readsPrec prec ('<': '<':s) = 
        case readsPrec prec s of 
            [(a,t)] -> case readList t of
                         [(as,ts)] -> case ts of
                                        ('>':'>':ss) -> [(RoseTree a as, ss)]
                                        _ -> []
                         _ -> []
            _ -> []
    readsPrec prec _ = []

    readList xs = let readList' ('[':ys) rs = case readsPrec 0 ys of
                                               [(r,zs)] -> readList' zs (r:rs)
                                               _ -> readList' ys rs
                      readList' (',':ys) rs = case readsPrec 0 ys of
                                               [(r,zs)] -> readList' zs (r:rs)   
                                               _ -> []
                      readList' (']':ys) rs = [(rs, ys)]
                      readList' _ _ = []
                  in readList' xs []
```
9. Ahora implementamos la funcion *main* para usar las clases de tipo precedentes:
```hs
main :: IO ()
main = do
    putStrLn "Enumeracion de meses"
    putStrLn $ show [January ..December]
    putStrLn "Enumaracion de meses impares"
    putStrLn $ show [January, March .. December]
    putStrLn $ "Igualando meses: January con sigo mismo : "
       ++ (show $ January == January)
       ++ " and January == with February : "
       ++ (show $ January == February)
    putStrLn $ "Using /= function"
    putStrLn $ "Meses que no son iguales, January con sigo mismo : "
       ++ (show $ January /= January)
       ++ " y January con February : "
       ++ (show $ January /= February)
    putStrLn $ "Comparando meses, January con si mismo : " 
        ++ (show $ January `compare` January) 
        ++ " y January con February : " 
        ++ (show $ January `compare` February)

    putStrLn ""
    putStrLn "Creando un arbol"

    let singleton = RoseTree 10 []
        tree = RoseTree 10 [RoseTree 13 [RoseTree 11 []], RoseTree 7 [], RoseTree 5 [RoseTree 3 []]]

    putStrLn ""
    putStrLn $ "Mostrando el singleton tree : " ++ show singleton
    putStrLn $ "Mostrando el arbol : " ++ show tree

    putStrLn ""
    putStrLn $ "Leemos lo que muestras -- show (read (show tree))"
    putStrLn $ "Singleton tree - " ++ show (read (show singleton) :: RoseTree Int)
    putStrLn $ "Tree - " ++ show (read (show tree) :: RoseTree Int) 
```
10. Construimos y ejecutamos la operacion
```hs
stack build 
stack exec -- working-type-classes
```
## Como funciona...
Clases de tipos proveen una buena abstraccion para definir comportamientos comunes a traves de los tipos de datos. Por ejemplo, la clase de tipos *Eq* se define como sigue:
```hs
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```
La clase de tipo precedente define un comportamiento para el tipo *a*. El comportamiento es un conjunto de funciones. La clase *Eq* especifica 2 funciones, equality (==) y non-equality (/=). Ambas funciones toman dos argumentos de tipo *a* y retornan Bool.

El standard Haskell provee definiciones para ambos (==) y (/=):
```hs
x == y = not (x /= y) -- note la definicion de (==)
x /= y = not (x == y) 
```
Puedes ver que el comportamiento de la igualdad es definido en terminos de la no-igualdad y viceversa. Para ser capaz de proveer una definicion significativa para nuestros tipo de datos. deberia ser suficiente proveer una definicion para (==) o para (/=), par que la definicion por defecto pueda llamar a otro operador. En nuestro caso, proveemos la definicion (==) para convertir el valor a Int usando la clase *Enum*. La definicion de Eq requiere la creacion de una instancia como sigue:
```hs
instance Eq Month where
  (==) month1 month2 = (fromEnum month1) == (fromEnum month2)
```
La definicion de *Show* y *Read* necesita mas atencion. *Show* necesita una funcion *show :: a -> String*. La funcion concatenara string con (++) es proporcinal al tamanio del string sobre el lado izquierdo, lo que no resulta eficiente. Para aligerar el problema, escribimos una funcion toSTring:: a -> String -> String. Este usara el segundo argumento para acumular los valores convertidos a string que es similar al patron worker. La clase Read de forma similar necesita una definicion de readsPrec. *readsPrec* retorna el tipo *ReadS*, que es equivalente a *String -> [(a, String)]*. La entrada es la representacion en String del tipo de datos, y el valor resultante es una lista singleton (lista con solo un item), con una tupla. La tupla resultante contiene el valor del tipo de datos y el string restante (despues de convertir el valor). Esto nos permite continuar parseando usando el resto del arbol. En la definicion de *Read* instanciado para *RoseTree*, definimos una funcion *readsPrec*, que comienza por escanear el string para el inicio <<, que marca el inicio de *RoseTree*. Tambien anotamos la instancia de *Read* para *RoseTree*:
```hs
instance Read a => Read (RoseTree a) where
```
Este indica que *Read* instanciada para *RoseTree a* esta definida  solo si *Read* instanciada para *a* esta tambien definida.