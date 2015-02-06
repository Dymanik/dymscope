---
title: Especificación del Lenguaje
author: Johan Gonzalez
documentclass: article
geometry: margin=1in
papersize: a4paper
---

\pagebreak

#Introducción

En este documento se especifica el lenguaje DymScope. Es un lenguaje que tiene como objetivo estudiar alcances y asociaciones, por lo que debe poder ejecutarse con alcance dinámico y estático, y asociaciones profundas y superficiales. 

#Estructura Léxica

##Espacios en Blanco

Los espacios en blanco están definidos por los caracteres ASCII de espacio, tabulación, salto de linea. 

##Comentarios

Existen dos tipos de comentarios:

  - **/*texto*/**  -- donde todo el texto entre /* y */ es ignorado.
  - **//texto** -- donde todo el texto desde // hasta el final de la linea es ignorado.

##Identificadores

Un identificador es una secuencia ilimitada de caracteres alfanuméricos, la cual debe comenzar con una letra.

Un identificador no puede ser igual a alguna palabra reservada o constante booleana.

##Palabras reservadas

Palabras del lenguaje que no pueden ser redefinidas ni usadas como identificador:

  - **function**
  - **return**
  - **proc**
  - **if**
  - **else**
  - **var**
  - **int**
  - **print**

##Tipos
Existen tres tipos básicos:

  - **int**
  - **function**
  - **proc**


##Literales
###Literales Numéricos (enteros)
Los literales enteros pueden ser expresados en decimal(base 10) y hexadecimal(base 16)

###Literales Booleanos
Las dos constantes booleanas:

  - **true**
  - **false**


##Operadores

```C
+ - / * = < > <= >= != == && || !
```

#Estructura del programa

Un programa esta definido como una serie de instrucciones y/o declaraciones, las cuales son ejecutadas secuencialmente. Las posibles instrucciones y declaraciones posibles son:

  - Declaracion de variable
  - Declaracion de función
  - Declaracion de procedimiento
  - Asignacion
  - Condicional if
  - LLamada a procedimiento
  - Retorno
  - Impresion de Valor


## Declaracion de variables

Introduce un identificador para referirse a un valor, este valor puede ser obtenido y modificado por su nombre a lo largo de su alcance.

```C
var identificador : tipo ;
```


## Declaraicion de Funciones y procedimiento

Introduce una serie de instrucciones que pueden ser invocados a través de su nombre y los argumentos requeridos por la función.

```C
function identificador ( argumentos ) { instrucciones }
proc identificador ( argumentos ) { instrucciones }
```

### Argumentos de Funciones y procedimientos

Es una lista separada por ',' ,posiblemente vacía de identificadores seguidos por su tipo.

```C
identificador : tipo , argumento
identificador : tipo
''
```

##Asignación

Asigna un nuevo valor a la variable.

```C
variable = expresión;
```

## Condicional If

Se evalúa la expresión booleana cond, y en caso de evaluar true, se ejecuta el bloque trueBlock, de lo contrario se ejecuta el bloque false en caso de existir.

```
if (cond) { trueblock } else { falseblock }
if (cond) { trueblock }
```

##Impresion de valor
Imprime en pantalla el valor pasado como argumento

```C
print ( expresión ) ;
```
##LLamada a procedimiento

Ejecuta el procedimiento identificador, con los valores de los argumentos. Los argumentos son evaluados de izquierda a derecha.

```C
identificador ( argumentos ) ;
```

###Argumentos de llamada

Es una lista separada por ',' de expresiones, que puede estar vacía.

```C
expresión , expresión
expresión 
''
```

##Retorno

Culmina la ejecuciones de la función o procedimiento dando como resultado la expresión especificada. La expresión es opcional

```C
return ;
return expresión ;
```
\pagebreak

#Ejemplo

```C

var n:int;
n=10;


function fib(n:int){
	if(n==0){
		return 1;
	}
	if(n==1){
		return 1;
	}
	return fib(n-1) + fib(n-2);
}

print(n);
print(fib(n));
```
