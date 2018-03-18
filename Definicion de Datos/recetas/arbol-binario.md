## Definiendo un arbol binario y haciendo un recorrido

En esta receta, veremos un tipo de dato que es definido recursivamente, Definiremos un arbol binario y exploraremos las funciones para recorrer el mismo.

### Comenzamos con

Creamos un proyecto nuevo *binary-tree-traverse* usando el template simple
```bash
stack new binary-tree-traverse simple
```
## Como lo hacemos...

1. Abrimos *src/Main.hs* y editamos.
2. Definimos el tipo de datos  para el arbol binario.
```hs
data BinaryTree a = Leaf
  | BinaryTree { left :: BinaryTree a 
                       , val :: a
                       , right :: BinaryTree a }
  deriving Show
```
3. Escribimos las funciones *helpers* que son *empty, singleton y node* para crear un arbol vacio y un arbol con un solo nodo, y componemos los dos arboles con un valor para crear un arbol nuevo.
```hs
empty :: BinaryTree a
empty = Leaf

singleton :: a -> BinaryTree a 
singleton x = BinaryTree Leaf x Leaf 

node :: BinaryTree a -> a -> BinaryTree a -> BinaryTree a
node l x r = BinaryTree { left = l, val = x, right = r}
```
4. Definimos el recorrido en profundidad  para nuestro arbol binario.
```hs
dfTraverse :: BinaryTree a -> [a]
dfTraverse Leaf = []
dfTraverse tree = dfTraverse (left tree) ++ [val tree] ++ dfTraverse (right tree)
```
5. Ahora, definimos recorrido en anchura para el arbol binario.
```hs
bfTraverse :: BinaryTree a -> [a]
bfTraverse Leaf = []
bfTraverse tree = bfTraverse1 [tree] [] []
  where
    bfTraverse1 [] [] xs = reverse xs
    bfTraverse1 [] q xs = bfTraverse1 (reverse q) [] xs
    bfTraverse1 (Leaf:ts) q xs = bfTraverse1 ts q xs
    bfTraverse1 (t:ts) q xs = bfTraverse1 ts (right t:left t:q) (val t:xs)
```
6. Creamos un arbol de ejemplo 
```hs
sampleTree :: BinaryTree Int
sampleTree = node l 1 r
  where
    l = node ll 2 rl
    r = node lr 3 rr
    ll = node lll 4 rll
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
7. Ahora usamos el ejemplo para hacer el recorrido en profundidad y anchura.
```hs
main :: IO ()
main = do
  let tree = sampleTree
  let inorder = dfTraverse tree
  let bfs = bfTraverse tree
  putStrLn "Recorrido primero en profundidad"
  print inorder
  putStrLn "Recorrido primero en amplitud"
  print bfs
```
8. Ahora construimos y ejecutamos
```bash
stack build
stack exec binary-tree-traverse
```
### Como funciona...

1. El arbol binario puede estar vacio o puede tener dos hijos arboles y un valor.
2. El arbol binario es parametrizado para un tipo de argumento. El tipo el argumento denota el tipo del valor de cada nodo almacenado.
3. El arbol binario es definido como un tipo suma.con las siguientes alternativas
	1. El arbol vacio es denotado por el constructor *Leaf*
	2. El nodo del arbol binario es del tipo producto (*BinaryTree*) implementado usando la sintaxis record, cons los siguientes campos
		- **left**:Este denota el arbol binario izquierdo
		- **val**: Este indica el valor del nodo
		- **right**: Este denota el arbol binario izquierdo
	3. Como *left* y *right* son del tipo *BinaryTree*, este es un tipo de dato recursivo.
4. La funcion helper *empty* crea un arbol vacio, que solo tiene una hoja sin valor alguno.
5. La funcion *singleton* crea un nodo con dos arboles hijos vacios
6. La funcion *node* toma un arbol binario *left* y *right* con un valor.
7. Es posible crear valores de *BinaryTree* usando directamente los constructores. Sin embargo, es mas facil muchas veces usar funciones *helper* que podamos definir.
8. El arbol de ejemplo es como sigue:
9. La receta define dos recorridos en profundidad y anchura. ambos retornan una lista de valores que indica el orden del recorrido.
10. En el recorrido en profundidad los pasos seguidos son:
	1. El arbol izquierdo es recorrido.
	2. Entonces, el nodo padre es visitado.
	3. Entonces, el arbol izquierdo es visitado de nuevo.
11. El recorrido en profundidad usa dos casos:
	1. El arbol esta vacio (su valor es *Leaf*). En este caso una lista vacia es retornada.
	2. El arbol tiene un arbol izquierdo, un valor, un arbol derecho. En este caso recorremos recursivamente por el nodo izquierdo y tomamos el valor del nodo y el resultado de recorrer el nodo derecho. En el ejemplo el orden es **[8,4,9,2,10,5,11,1,12,6,13,3,14,7,15]**
12. En el recorrido en anchura, todos los nodos del mismo nivel son visitados antes de visitar los nodos del siguiente nivel.
13. El recorrido en anchura es implementado usando un patron **worker**:
	1. *bfTraverse1* asume que esta recorriendo un conjunto de nodos primero en anchura de un cierto nivel.
	2. Como este visita nodos, este recolecta ambos hijos y los pone en una cola.
	3. Cuando la entrada de nodos esta terminada, la colar es invertida y pasada a *bfTraverse1* en una recursion. La cola invertida se implementa como una lista, y el elemento mas nuevo es adicionada y removido del frente de la lista.
	4. Para el ejemplo el recorrido en anchura de la receta deberia lucir como: **[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]**