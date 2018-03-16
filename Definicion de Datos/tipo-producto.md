##Definiendo un Tipo Producto

En esta receta veremos tipos producto. Definiremos tipos de datos simples con 2 parametros, y haremos diferentes experimentos con ellos.

### Creando el proyecto

Creamos un nuevo proyecto llamado **product-type**  usando el template simple de Stack:

```bash
stack new product-type simple
```
### Pasos a seguir ...

1.Abrir **src/Main.hs** para editar.
2.Adiciona la definicion de datos para tipos producto:
```haskell
data Product1 = Product1 Bool deriving Show
data Product2 = Product2 Bool Bool deriving Show
data Product3 a = Product3 a Bool deriving Show
data Product4 a b = Product4 a b deriving Show
```
3.Cambiar la funcion main para usar los tipos producto definidos antes, creamos instancias e imprimimos sus valores 
```haskell
main :: IO ()
main = do
  putStrLn "Product1: Tipo producto simple"
  putStrLn $ show $ Product1 True
  putStrLn $ show $ Product1 False

  putStrLn "Product2: Tipo Product con dos campos"
  putStrLn "Product2 tiene dos campos booleanos. cada uno solo puede ser true o false"
  putStrLn $ show $ Product2 True True
  putStrLn $ show $ Product2 True False
  putStrLn $ show $ Product2 False True
  putStrLn $ show $ Product2 False False

  putStrLn "Product3: Tipo producto con dos campos, un es parametrico (Int)"
  putStrLn "La cardinalidad de Product 3 es la cardinalidad de Int multiplicada por 2"
  putStrLn $ " que es " ++ (show ( 2 * (fromIntegral (maxBound :: Int) - fromIntegral (minBound :: Int) + 1 )))
  let product3 = Product3 10 True :: Product3 Int
  putStrLn $ show product3

  putStrLn "Product4: Tipo producto parametrizado para 2 tipos (Int Bool)"
  putStrLn "Es equivalente a Product 3 en su parametros"
  putStrLn $ show $ (Product4 10 True :: Product4 Int Bool)
```
4. Construimos y ejecutamos el proyecto:
```bash
stack build
stack exec product-type
```
---
###Como Funciona
1.Definimos **Product1** con un constructor de datos **Product1 :: Bool -> Product1**. Cuantos valores pueden ser construidos por Product1? Como este tiene tipo **Bool** Puede tener solo dos valores **Product1 True** y **Product1 False**. Este es el tipo Producto mas simple que hemos definido.
2.A continuacion definimos **Product2** que toma dos valores Booleanos. Para este constructor se tiene cuatro valores: **Product2 True True, Product2 True False, Product2 False True, Product2 False False **.(F)
3.A continuacion creamos el tipo de dato parametrico **Product3** que toma un tipo de dato **a** y tiene un campo adicional de tipo **Bool**. En el ejemplo, construimos una instancia de **Product3 Int** Este tipo tiene 2 campos, uno para el tipo **Int** y otro para el tipo **Bool**. Sabemos que Bool tiene dos posibles valores. Para encontrar el numero de posibilidades para Int, usamos **maxBound y minBound** para conseguir el maximo y minimo numero de valores.
4. Para **Product4**, se toman dos argumentos, a y b. En el ejemplo usamos **Int** y **Bool** como tipos de los argumentos . Aqui el numero de posibles valores de **Product4 Int Bool** es la misma que **Product3 Int**.
