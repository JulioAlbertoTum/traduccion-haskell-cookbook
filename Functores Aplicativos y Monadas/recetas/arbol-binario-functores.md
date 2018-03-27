## Un Arbol Binario con Functores

En este receta, tenemos las instancias de *Functor* para *Maybe, Either*, y *List*. Incluso definimos la instancia de *Functor*. En esta receta, crearemos un tipo de dato *Binary Tree* y definimos una instancia *Functor* para esto.

## Como lo hacemos...
1. Creamos una proyecto nuevo *binary-tree-functor* usando el template *simple*:
```bash
stack new binary-tree-functor simple
``` 
2. Abrimos "src/Main.hs" que usaremos para nuestro proposito.
3. Adicionamos los siguientes imports:
```hs
module Main where
import Data.Functor
```
4. Define las funciones *binary tree* y *utility* para crear el arbol:
```hs
-- El arbol puede estar vacio (Leaf) o ser un nodo con un valor
-- left y right arboles
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show Eq)

-- Creamos un arbol dado un valor, left tree y right tree
node :: Tree a -> a -> Tree a -> Tree a
node l x r = Node l x r

-- Introducimos un valor dentro un arbol nuevo (nodo con arboles izq y der vacios)
singleton :: a -> Tree a
singleton x = Node Leaf x Leaf
-- 
```
5. Definir una instancia de un *Functor*  para este arbol binario. Tenemos que considerar dos casos. Lo primero es que hacer cuando el arblo esta vacio. Es obvio que para un arbol vacio, la funcion de aplicacion no tiene sentido y deberia retornar in arbol vacio. En el segundo caso, tenemos un Nodo con un valor y 2 subarboles, que son left tree y right tree. La aplicacion de la funcion para el Functor transformara el valor, y entonces podemos usar la definicion de *fmap*  recursivamente para transformar los subarboles tambien:
```hs
instance Functor Tree where
	fmap _ Leaf = Leaf
	fmap f (Node left value right) = Node (fmap f left) (f value) (fmap f right)
```
6. Escribimos ejemplos de codigo para testear nuestras instancias. Primero, crearemos un ejemplo de un arbol de enteros:
```hs
sampleTree :: Tree Int
sampleTree :: node l 1 r where
	l = node ll 2 rl
	r = node lr 3 rr
	ll = node lll 4 lll
	rl = node lrl 5 rrl
	lr = node llr 6 rlr
	rr = node lrr 7 rrr
	lll = singleton 8
	rll = singleton 9
	lrl = singleton 10
	rrl = singleton 11
	llr = singleton 12
	rlr = singleton 13
	lrr = singleton 14
	rrr = singleton 15
```
7. En la funcion *main*, usaremos la funcion shiw a traves de *Functor* para convertir un arbol binario de enteros a un binario de strings. Despues usamos la funcion read para convertir este arbol de nuevo  un arbol de enteros. Para verificar si la implementacion de la funcion es correcta, compararemos ambos arboles.
```hs
main :: IO ()
main = do
  let intTree = sampleTree
      -- convertir arbol de enteros a arbol de cadenas
      stringTree = fmap show intTree
      -- usamos read para convertir de nuevo el arbol de enteros
      intTree1 = fmap ( read :: String -> Int) stringTree
  putStrLn "Original Tree"
  print intTree
  putStrLn "Arbol de enteros a arbol de cadenas"
  print stringTree
  putStrLn "Es el Arbol de cadenas convertido al arbol original?"
  print $ intTree == intTree1
```

### Como funciona
En esta receta, creamos una instancia de *Functor* para el *arbol binario* que definimos. Cuando creamos una instancia de una *Functor*, debemos seguis las siguientes leyes del functor:
#### Leyes del functor
Una aplicacion de una funcion identidad *id* debe ser el mismo de vuelta:
```hs
fmap id == id
```
Aplicando 2 funciones del mismo tipo de dato en una secuencia deberia ser lo mismo que aplicar una composicion de 2 funciones
```hs
fmap (p . q) == fmap p . fmap q
```
Para el *arbol binario* que hemos definido, estas leyes se siguen. Si nosotros aplicamos la funcion *id* entonces aplicamos este al valor en el nodo y subarboles. Una vez que *id* no cambia el valor y preservamos la estructura del arbol, una aplicacion de uan funcion identidad a nuestro arbol binario usando fmap deberia retornar el mismo arbol. Para la segunda ley verificamos esta a traves de ejemplos de codigo en la funcion *main*. La composicion de *read . show* es una funcion identidad (dado que hacemos una conversion de ida y vuelta). Para aplicaciones sucesivas de fmap con show y  read con un arbol que convertimos de enteros a arbol de cadenas, y de nuevo de cadenas a enteros. Verificamos correctitud de nuestra implementacion verificando la igualdad del arbol original y del converso.