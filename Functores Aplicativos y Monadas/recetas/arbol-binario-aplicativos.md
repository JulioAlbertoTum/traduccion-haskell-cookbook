## Arbol binario como aplicativo
En este ejemplo, definiremos un arbol binario y definiremos como hacerlo como una instancia de la clase de tipo *Applicative*.

### Como lo hacemos... 
1. Creamos un nuevo proyecto *binary-tree-applicative* usando stack y la plantilla simple.
2. Abrimos *src/Main.hs*; adicionaremos nuestro receta a este archivo.
3. Despues de la definicion de un modulo inicial, adicionamos los siguientes imports:
```hs
import Data.Functor
import Control.Applicativ
``` 
4. Definimos el arbol binario y adicionamos la instancia *Functor* tambien:
```hs
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show, Eq)

instance Functor Tree where
	fmap _ Leaf = Leaf
	fmap f (Node left value right) = Node (fmap f left) (f value) (fmap f right)
```
5. Ahora, define la instancia *Applicative* para el arbol binario. Note la definion recursiva para *pure*, produciendo un arbol infinito:
```hs
instance Applicative Tree where
    pure x = let t = Node t x t
             in t

(<*>) Leaf _ = Leaf
(<*>) _ Leaf = Leaf
(<*>) (Node lf f rf) (Node la a ra) = Node (lf <*> la) (f a) (rf <*> ra)
```
La funcion *pure* crea un arbol infinito, mientras que para la <\*> definicion, siempre retornamos un arbol vacio si uno de los argumentos esta vacio. Si el valor es del tipo *Node left v right*, entonces aplicamos la funcion al valor y recursivamente aplicamos la funcion left encapsulada *lf* a *la* y *rf* a *ra*.

6. Adicionamos una funcion para crear un ejemplo de arbol en la funcion *main* testeando nuestras instancias de *Applicative* y leyes de *Applicative*:
```hs
singleton :: a -> Tree a
singleton x = Node Leaf x Leaf

node :: Tree a -> a -> Tree a -> Tree a
node l x r = Node l x r

sampleTree :: Int -> Tree Int
sampleTree b = node l b r where
  l = node ll (b+1) rl
  r = node lr (b+2) rr
  ll = node lll (b+3) rll
  rl = node lrl (b+4) rrl
  lr = node llr (b+5) rlr
  rr = node lrr (b+6) rrr
  lll = singleton (b+7)
  rll = singleton (b+8)
  lrl = singleton (b+9)
  rrl = singleton (b+10)
  llr = singleton (b+11)
  rlr = singleton (b+12)
  lrr = singleton (b+13)
  rrr = singleton (b+14)

main :: IO ()
main = do
  let intTree1 = sampleTree 1
      intTree2 = sampleTree 15
      finalTree = (+) <$> intTree1 <*> intTree2
  putStrLn "First Tree"
  print intTree1
  putStrLn "Second Tree"
  print intTree2
  putStrLn "Final Tree"
  print finalTree
  putStrLn "Verificando leyes de los aplicativos"
  putStrLn "Identificando Ley: pure id <*> v == v"
  putStrLn "pure id <*> intTree1 == intTree1"
  print $ (pure id <*> intTree1) == intTree1
  putStrLn "Homomorfismo: pure f <*> pure x == pure (f x)"
  putStrLn "Esta propiedad no se puede testearla, como pure producer un arbol infinito"
  putStrLn "Intercambia: u <*> pure y == pure ($ y) <*> u"
  putStrLn "Esta propiedad no se puede testear, como pure produce un arbol infinito"
  putStrLn "Composicion: pure (.) <*> u <*> v <*> w == u <*> (v <*> w)"
  let square x = x * x
      double x = x + x
  putStrLn "(pure (.) <*> pure square <*> pure double <*> intTree1) == (pure square <*> (pure double <*> intTree1))"
  putStrLn $ (pure (.) <*> pure square <*> pure double <*> intTree1) == (pure square <*> (pure double <*> intTree1))
```
## Como funciona
El *Applicative* debe seguir las siguientes leyes:
- Ley de identidad: Una aplicacion de identidad no debe cambiar los datos:
```hs
pure id <*> v == v
```
- Homomorfismo: Una aplicacion de una funcion a un dato (f x) es equivalente a la aplicacion de *Applicative* de la funcion aplicada a *pure* data (data induce un *applicative*):
```hs
pure f <*> pure x == pure (f x)
```
- Intercambiable: Es equivalente a decir
```hs
f (a -> b) -> f a == f b -- deberia ser equivalente a
f((a -> b) -> b) -> f (a -> b) == f b:
u <*> pure y = pure ($ y) <*> u
```
- Composicion: Estipula que un operador de composicion *applicative* es similar a la funcion de composicion (.):
```hs 
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
```
Aqui, la aplicacion de la composicion *pure (.) <*> u <*> v* sobre *w* es lo mismo que aplicar u sobre *v <*> w*.
Estas no son solo leyes; Ellas nos dan una forma de implementacion cruzada para verificar nuestra implementacion es correcta. Ellas tambien nos permiten razoznar sobre una instancia de "Applicative". Estas leyes permiten que un *Applicative* embeba una computacion y se mueva libremente.
En el caso de un arbol binario, la primera ley *pure id <*> v == v* deberia guardarse. Para ser capaz de satisfacer esta ley, tenemos que *pure x* sea una instancia de una arbol binario y infinitamente recursivo. En este caso, tomaremos el elemento y contruiremos un arbol infinito donde todos los nodos tengan un valor *x* y ambos subarboles tengan la misma raiz. Una funcion *pure* esta definida como sigue:
```hs
pure x = let tree = Node tree x tree
     in tree
```
La definicion de <*> es completamente sencilla. Extrae la funcion del primer argumento f (a -> b) y aplica este al valor contenido con el nodo. Entonces, este toma el subarbol izquierdo del primer argumento y aplica este la subarbol derecho del segundo argumento. Esto se repite en el arbol derecho.
Este proceso continua  recursivamente hasta que una hoja es encontrada en un subarbol.Cuando una hoja es encontrada, no hay necesidad de hacer una accion y el resultado es:
```hs
(<*>) Leaf _ = Leaf
(<*>) _ Leaf = Leaf
(<*>) (Node lf f rf) (Node la a ra) = Node (lf <*> la) (f a) (rf <*> ra)
```
La receta tiene un ejemplo para provar que nuestra instancia de *Applicative* sigue la primera y cuarta ley. Sin embargo, como la definicion de pure resulta en un arbol infinito, no podemos ejecutar este en el programa. Sin embargo podemos mostrar que por razonamiento, el estado del homomorfismo es el siguiente:
```hs
pure f <*> pure x == pure (f x)
```
Considera la definicion del arbol binario en la receta; podemos escribir la sentencia precedente sustituyendo la definicion de pure:
```hs
tf <*> tx == tfx -- Nota: esto no es codig haskell
where 
  tf = Node tf f tf
  tx = Node tx x tx
  tfx = Node tfx (f x) tfx

-- La expresion anterior seria
Node tf f tf <*> Node tx x tx = Node (tf <*> tx) (f x) (tf <*> tx) -- definicion recursiva para eval tfx
                              = Node tfx (f x) tfx
                              = tfx
```
De forma similar el intercambio tambien se puede probar.
Lo importante a notar es que un *Applicative* perimte secuenciar, sin necesitar conocer los resultados intermedios. O sea un Applicative es mas fuerte que un functor y mas debil que una monada.
