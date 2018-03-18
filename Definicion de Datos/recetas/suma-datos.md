## Definicion de tipo suma

La suma de tipos son equivalentes a variantes (o uniones en C). Sin embargo, en haskell es mucho mas que eso. Es tambien llamada **tagged union**. El tipo suma mas simple es **Bool**, que puede tener 2 valores **True** y **false**. En esta receta definimos tipos suma y los usamos en nuestros ejemplos.

## Comenzamos con...

Creamos uun proyecto llamado sum-type usando el template simple de Stack
```bash
stack new sum-type simple
```
### Que hacemos...

1. Abrimos *src/Main.hs* para editar.
2. Adicionamos ell siguiente tipo de datos para representar los dias en la semana:
```haskell
data Days = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving Show
```
3. Ahora, adicionamos el tipo Variant que toma 5 tipos de argumentos:
```haskell
data Variant a b c d e = Variant0
  | Variant1 a
  | Variant2 b
  | Variant3 c
  | Variant4 d
  | Variant5 e
  deriving Show
```
4. Ahora, usamos los tipos anteriores en la funcion main:
```hs
main :: IO ()
main = do
  putStrLn $ "Sum Type 1: Mostrando dias de la semana"
  putStrLn $ show [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]
  putStrLn $ "El tipo Days puede tener solo 7 valores"
  putStrLn "Sum Type 2: Variant con 5 posibles constructores de datos"
  putStrLn "Cada constructor contribuye al numero de posibles valores"
  putStrLn " de tipos a, b, c, d, e, o f"
  let v0 = Variant0 :: Variant Int Float Double Char String
      v1 = Variant1 10 :: Variant Int Float Double Char String
      v2 = Variant2 11.0 :: Variant Int Float Double Char String
      v3 = Variant3 12.0 :: Variant Int Float Double Char String
      v4 = Variant4 'A' :: Variant Int Float Double Char String
      v5 = Variant5 "Haskell" :: Variant Int Float Double Char String
  putStrLn "Mostrando todas las variantes"
  putStrLn $ show [v0, v1, v2, v3, v4, v5]
  putStrLn "Variant0 tiene solo un valor, sin embargo su tipo esta completamente cualificado"
```
5. Ahora construimos y ejecutamos
```bash
stack build
stack exec sum-type
```
### Como funciona...

1. Primero, definimos un tipo de datos *Days*. Que tiene 7 alternativas (*Sunday* a *Saturday*). El tipo *Days* puede tomar hasta 7 valores.
2. A continuacion, definimos el tipo *Variant*. El tipo *Variant* tiene 5 tipos de argumentos. En el ejemplo usamos *Int, Float, Double, Char, String*. El tipo de dato definido con el constructor *Variant1 hasta Variant5* con un campo para un tipo correspondiente. *Variant0* no tiene un campo. Por tanto el numer de posibles tipos que Variant puede tener es la suma de  todos los posibles valores que cada tipo puede tomar. Adicionalmente , pudemos adicionar un valor para *Variant0*.
3. Una vez que el numero de posibles valores es la suma de alternativas, este es llamado **tipo suma**.
4. Este tipo no se encuentra en lenguajes como C/C++. La union es lo mas cercano que se puede tener al tipo suma en estos lenguajes.