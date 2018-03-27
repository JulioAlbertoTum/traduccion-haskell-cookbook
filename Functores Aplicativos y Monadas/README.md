# Trabajando con Functores, Aplicativos y Monadas
En este capitulo, veremos las siguientes recetas:
- [Trabajando con Functores](recetas/trabajando-functores.md)
- [Arbol Binario como Functor](recetas/arbol-binario-functores.md)
- Trabajando con Aplicativos
- Arbol Binario como Aplicativo
- Trabajando con Monadas
- Lista como Monada
- Escribiendo INI Parser:
	- Parser como Functor
	- Parser como Aplicativo
	- Parser como monada
- Errores y manejo de Excepciones
---
## Introduccion

Hemos trabajado sobre funciones, funciones de alto orden, y tambien con tipos de datos en haskell. Hemos visto funciones tales como *map* y *filter* en el contexto del tipo de dato lista. En muchos de estos ejemplos, hemos tomado funciones que operan sobre tipos de datos *a* y son aplicados en el contexto de listas de tipo *a*. Mira la siguiente definicion de map:
```hs
map :: (a -> b) -> [a] -> [b]
```
Puedes ver claramente que hemos tomado una cancion que opera sobre tipos de datos *a* y produce *b*, y hemos convertido esta con una funcion que toma una lista *a* y produce una lista *b*. En lugar de una lista de *a*, podriamos pensar en algun tipo de dato parametrico como *T a*. Ahora podemos reescribir la declaracion de *maps* como sigue:
```hs
map :: (a -> b) -> T a -> T b
```
En resumen, la definicion precedente de *map* se aplica a cualquier tipo de dato *T a*, dada una funcion que opera sobre *a*. Bero ahora como definimos map? Como sabemos que hacer para que el tipo de dato *T a* produzca el tipo de dato *T b*?

En este capitulo, responderemos estas preguntas. A traves de estas preguntas descubriras que no solo hacer tales definiciones crea un concepto generico, al mismo tiempo, abstraen las propiedades inherentes de un tipo de dato tal como *T a* para adaptar a una funcion, tal como *a -> b* para si misma.

La creacion de tales estructuras abstractas inherentemente hacen que el programa Haskell sea mas facil de expresar y comprender. La precedente operacion *map-like* es una propiedad de un *Functor*. En este capitulo, tambien veremos *Applicatives* y *monads*, que son algunos de los mas usados e importantes, ademas hablaremos de clases de tipos en Haskell.