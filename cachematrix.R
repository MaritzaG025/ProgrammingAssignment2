##se crearán dos funciones: makeCacheMatrix y cacheSolve. 
##cacheSolve()se puede calcular y almacenar la matriz inversa del argumento  'x' de entrada, si es de tipo makeCacheMatrix(). 
##Debido a que los elementos de la lista en makeCacheMatrix()se definen con nombres, podemos acceder a estas funciones con la $

##La función makeCacheMatrix crea un objeto "matriz" especial que puede almacenar en caché su matriz inversa.

makeCacheMatrix <- function(x = matrix()) { ## x se inicializa como argumento de la función makeCacheMatrix y su valor predeterminado es una matriz vacía.
  inv <- NULL           ##inv se establece en NULL, inicializándolo como un objeto dentro del entorno makeCacheMatrix ()
  set1 <- function(y) { ##y es el argumento de la función set1 y es una matriz.
    x <<- y             ##Asigna el argumento de entrada al objeto x en el entorno principal y
    inv <<- NULL        ##Esta línea de código borra cualquier valor inv que haya sido almacenado en caché por una ejecución previa de cacheSolve().
  }
  get1 <- function() x                       ##get1 es una función que capta el valor de la matriz x
  setsolve <- function(solve) inv <<- solve  ## se utiliza el operador de doble asignación para asignar el argumento de entrada al valor de la inversa inv en el entorno principal.
  getsolve <- function() inv                 ##getsolve es una función que capta el valor de la inversa inv
  list(set1 = set1, get1 = get1,             ##Las funciones set1, get1, setsolve, getsolve se asignan como elementos de una lista,  y lo devuelve al entorno padre. 
       setsolve = setsolve,
       getsolve = getsolve)
}

## La función cacheSolve calcula la inversa de la "matriz" especial devuelta porla función makeCacheMatrix.
## Si ya se ha calculado la inversa (y la matriz no ha cambiado), entonces la solución de caché debería recuperar la inversa de la caché.

cacheSolve <- function(x, ...) {         ## La función cacheSolve toma el argumento x, y el argumento ... que pasar argumentos adicionales a la función.
  inv <- x$getsolve()                    ## La función intenta recuperar la inversa inv del objecto pasado como argumento. 
  if(!is.null(inv)) {                    ## comprueba si el resultado no es NULL.
    message("obteniendo datos en caché") ## De ser así, arrojá el mensaje "obteniendo datos en caché".
    return(inv)                          ## Devuelve al entorno principal.
  }
  data <- x$get1()        ##Si el resultado es NULL, obtenemos la matriz del objeto de entrada. Y la guardamos en la variable data.
  inv <- solve(data, ...) ## Calculamos la inversa. 
  x$setsolve(inv)         ##Establece la inversa del objeto de entrada.
  inv                      ## Devuelve el valor de la inversa de 'x' al entorno principal imprimiendo el objeto de la inversa.
}