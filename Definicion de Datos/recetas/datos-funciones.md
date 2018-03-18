## Definiendo datos con funciones

Hasta ahora, hemos visto tipos de datos que toman el valor de otro tipo de datos (*simples* o *complejos*). Una vez que las funciones en Haskell son valores de primera clase, podemos usar funciones en la definicion de nuestros tipos de datos. En esta seccion definimos 2 tipos de datos que usan funciones como uno de los campos.

El primer tipo de datos encapsula una funcion *f :: a -> b*, mientras que el segundo tipo de datos es una estructura recursiva interesante.

### Empezamos con

Usamos el template simplle de stack para crear un nuevo proyecto llamado *data-type-with-function*, e ingresamos al directorio.
```bash
stack new data-type-with-function simple
```
### Como lo hacemos ...
1. Abrimos *src/Main.hs* para editar.
2. Adicinamos un nuevo tipo de dato *Func a b* para representar la funcion *f :: a -> b* 
```hs
newtype Func a b = Func (a -> b)
```
3. Adicionamos una funcion *compose. La funcion toma 2 funciones y las compone para dar la salida de la primera funcion a la siguiente:
```hs
compose :: Func a b -> Func b c -> Func a c
compose (Func f) (Func g) = Func (g . f)
```
4. Ahora, adicionamos una funcion *apply*; este toma nuestro tipo de dato *Func* y aplica un argumento a este:
```hs
apply :: Func a b -> a -> b
apply :: (Func f) a = f a
```
5. Ahora, define un tipo de dato *Fix*; este toma una funcion como un argumento e intenta definirlo recursivamente para aplicar a si misma la funcion:
```hs
newtype Fix f = Fix (f (Fix f))
```
6. Ahora definimos  el tipo *Ghost*; que toma un argumento. Sin embargo, este no lo usa en su definicion:
```hs
data Ghost a = Ghost deriving Show
```
7. Ahora usaremos estos tipos en la funcion main. Definiremos 2 funciones -- *square*, que eleva al cuadrado un numero dado, y *sqrti*, que es la raiz cuadrada de un entero. Envolveremos esta con nuestro tipo de dato *Func* y la compondremos. Como un resultado de la aplicacion de la composicion deberiamos tener el mismo entero de vuelta.
8. A continuacion usaremos el tipo de dato *Fix* con *Ghost* para ver como Fix se aplica recursivamente a si misma.
9. La funcion *main* es como sigue:
```hs
main :: IO ()
main = do
  let square x = x * x
  let sqrti = floor . sqrt . fromIntegral

  let squareF = Func square
  let sqrtF = Func sqrti
  
  let idF = compose squareF sqrtF

  putStrLn "Componiendo  las funciones cuadrado y raiz cuadrada"
  putStrLn " es una identidad. Aplicando un valor este no deberia cambiar"
  print $ apply idF 3

  let x = Ghost
      y = Fix x 
      Fix z = y

  putStrLn "Original value is "
  print x
  putStrLn "Despues de fixing, "
  print z
```  
10. Construimos y ejecutamos 
```bash
stack build
stack exec -- data-type-with-function
```
### Como funciona...

1. El primer tipo dato *Func* toma dos argumentos, *a* y *b*. La definicion el dato simplementa encapsula este dentro como *Func (a -> b)*. Aqui *(a -> b)* representa el tipo funcion que toma un argumento *a* y produce un valor de tipo *b*
2. La funcion  *compose* simplemente extrae la funcion encapsulada en *Func a b* y *Func b c* y las compone con la funcion *(.)*
	- La funcion (.) es llamada *composition*, y es usada para componer funciones sin tener que espeficicarlas. Esto pude ser simplemente definido como (.) *g f x = g (f x)*. Tambien, nota que cuando usamos la funcion composicion, no tenemos que especificar un argumento o funcion. Esto es llamado programacion con estilo de *punto libre*. A veces un estilo punto libre puede ser mas claro y entendible.
3. La funcion *apply* extrae la funcion y aplica un argumento a esta.
4. El tipo de dato *Fix* es interesante.Es recursivo y especial porque este se aplica el tipo del argumento a si mismo en su definicion. Mira cuidadosamente el tipo de dato en el uso en todos los pasos. Nota que el tipo de argumento  *a* que *Fix* necesita no es un tipo simple. Es un tipo que necesita otro tipo de argumento para este. Puedes inspeccionar este con las siguientes lineas de codigo
```bash
*Main> :i Fix
type role Fix nominal
newtype Fix (f :: * -> *) = Fix (f (Fix f))
-- Defined at  src\Main.hs:13:1
```
5. El tipo de dato *(f :: \* -> \*)* en la salida de GHCi indica que f es un tipo que necesita otro tipo como una entrada a este.
	- Primero definiremos *x* como un vinculo para el tipo *Ghost* y creamos un valor de *Fix* para aplicar x a este:
	```hs
	let x = Ghost
	y = Fix x
	```
6. Entonces, trataremos de extraer el valor en la definicion de *Fix*, que es, la parte testada de la definicion  *Fix f (Fix f)*. Podemos hacer esto usando el siguiente codigo:
```hs
let Fix z = y
```
7. Entonces, si verificamos el tipo de *z* , tendremos la siguiente salida:
```bash
*Main> :t z
z :: Ghost (Fix Ghost)
```
	- Esto puede sorprender, pero podemos hacer un acercamiento visual.El tipo *Ghost* que pasamos a *Fix* es aplicado a si mismo *Fix Ghost*. Esto produce un tipo recursivo.
	- El tipo Fix esta inspirado por su contraparte la funcion *fix :: (a -> a) -> a*. Esta representa un punto fijo y es una funcion muy util para entender la recursion.
8. Finalmente, el tipo *Ghost* parece curioso tambien. Este tima un tipo en su argumento pero no lo usa en su definicion. Aqui este es llamado un **Phantom Type**. Son muy utiles en programas para adicionar notacion de tipo a nuestra definicion.