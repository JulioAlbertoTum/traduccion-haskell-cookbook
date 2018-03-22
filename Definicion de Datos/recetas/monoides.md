## Trabajando con Monoides
Monoides es un tipo de Clases muy importantes y de amplio uso. Un monoide asume dos comportamientos:
1. Este tiene un valor por defecto o vacio del tipo de datos.
2. Da dos valores del tipo de datos, pueden se combinados para crear un solo valor.
Un ejemplo simple de un Monoide es Integer. Podemos definir un valor vacio de un entero como 0. Podemos entonces usar *addition* como una operacion para combinar dos *Integers*.En esta receta, definimos un tipo de datos *Option* y definimos una instancia para Monoide.

### Comenzamos 
Creamos un nuevo proyecto llamado *working-monoid* con la plantilla *simple* usando stack
```bash
stack new working-monoid simple
```
Vamos al directorio creado

### Como comenzar ...
1. Comenzamos editando *src/Main.hs*. Adicionamos *import Data.Monoid* en la cabecera del archivo. Este modulo contiene la definicion de la clase de tipos Monoide. 
2. Definimos el tipo de datos *Option*. Los datos contenidos son un campo para un Booleano y una lista de string:
```hs
data Option = Option {boolOption :: Bool, selections :: [String] } deriving Show
```
3. Definimos la instancia de *Monoid*. La clase *Monoid* necesita definir un minimo de 2 funciones, mempty, y mappend:
```hs
instance Monoid Option where
	mempty = Option False []
	(Option b1 s1) `mappend` (Option b2 s2) = Option (b1 || b2) (s1 ++ s2)
```
4. Usando el tipo de datos *Option* y su instancia de *Monoid* en la funcion *main*:
```hs
main :: IO ()
main = do
  putStrLn "Definimos opciones por defecto"
  let defaultOptions = mempty :: Option
  putStrLn (show defaultOptions)
  let option1 = defaultOptions `mappend` (Option True [])
      option2 = option1 `mappend` (Option False ["haskell"])
      option3 = option2 `mappend` (Option True ["cookbook"])
  putStrLn $ "Adicionamos el flag True - " ++ show option1
  putStrLn $ "Adicionamos False flag, y selection \"haskell\" - " ++ show option2
  putStrLn $ "Adicionamos True Flag, y selection \"cookbook\" - " ++ show option3

  putStrLn $ "Contatenamos todas las opciones"
  putStrLn $ "Resultado de la concatenacion - " ++ show (mconcat [defaultOptions, option1, option2 ])
```
### Como funciona...
La clase *Monoid* se define como:
```hs
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
```
La funcion *mempty* define una valor por defecto. La funcion *mappend* define como es el resultado de combinar dos valores de *a*, obtendremos un solo valor de tipo *a*. mconcat indica que podemos combinar todos los valores en la lista para producir una solo valor de tipo a.  Para definir una instancia de *Monoid*, necesitamos definir al menos *mempty y mappend*:
**Leyes de los Monoides** .- Las instancias de monoides deben seguir estas leyes:
- **mappend x mempty** = x (adicionando el valor por defecto no deberia cambiar el valor)
- **mappend mempty x** = x (adicionando un valor al valor por defecto es el mismo valor)
- **mappend x (mappend y z)** = mappend (mappend x y) z (asociatividad)
- **mconcat xs** = foldr mappend mempty (Concatenacion es equivalente a un foldr con mempty como valor por defecto y mappend como operacion de combinacion)

Para el tipo de datos *Option*, definimos *mempty* para ser *Option* con un flag booleano y un lista vacia para las selecciones. Cuando adicionamos, nosotros usamos *OR* para combinar valores booleanos, y *selection* strings son adicionados. Podemos confirmar que estas siguen las leyes del Monoide.

Monoides son muy utiles y aparecen en muchos lugares mientras programamos. De hecho, algunas veces, este puede tener mas de dos definiciones para un Monoide Por ejemplo, podemos definir Monoide para un *Ingeter* usando el valor por defecto *0* y la adicion como operacion *append*, o usando por defecto el valor 1 y la multiplicacion como funcion *append*. En tal caso, podemos envolver el tipo de datos dentro otro tipo de datos y proveer una alternativa como instancia de Monoide.

