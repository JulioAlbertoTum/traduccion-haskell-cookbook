#Definiendo Datos

En este capitulo, veremos las siguientes recetas:

- Definiendo un tipo producto
- Definiendo un tipo suma
- Definiendo un arbol binario y recorriendo el mismo
- Definiendo datos con funciones
- Usando Maybe
- Usando Either
- Trabajando con clases de tipo
- Trabajando con Monoides

---

##Introduccion

En el ultimo capitulo, vimos funciones, recursiones, y funciones de alto orden. Este capitulo veremos 
otro aspecto importante de Haskell. Los tipos de datos en Haskell son muy expresivos y son usados para crear
estructuras de datos muy intuitivas. Hemos visto que haskell trabaja por reduccion o calculo de valores a
a partir de expresiones (que son formadas aplicando valores a funciones, etc). Para cada valor, hay algun tipo asociado a este. De hecho podemos ver que cada tipo representa una coleccion o conjunto de valores.

En este capitulo, veremos tipos basicos algebraicos. El termino tipo algebraico viene de la asociacion entre los valores de un tipo y una operacion algebraica tal como suma o producto. Veremos tambien tipos definidos recursivamente, donde el tipo  es incluido  en la definicion del tipo mismo. Ademas tambien veremos tipos parametricos. Veremos dos tipos de uso comun en Haskell **Maybe** y **Either**.

Finalmente, introduciremos el concepto de **Clases de Tipos**. Explicaremos las clases mas basicas en Haskell, que son **Show**, **Eq**, **Ord** y **Read**. Tambien mostraremos como crear una instancia de un **monoide**, otro clase muy util. 