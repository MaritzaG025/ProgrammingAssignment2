##se crear�n dos funciones: makeCacheMatrix y cacheSolve. 
##cacheSolve()se puede calcular y almacenar la matriz inversa del argumento  'x' de entrada, si es de tipo makeCacheMatrix(). 
##Debido a que los elementos de la lista en makeCacheMatrix()se definen con nombres, podemos acceder a estas funciones con la $

##La funci�n makeCacheMatrix crea un objeto "matriz" especial que puede almacenar en cach� su matriz inversa.

makeCacheMatrix <- function(x = matrix()) { ## x se inicializa como argumento de la funci�n makeCacheMatrix y su valor predeterminado es una matriz vac�a.
  inv <- NULL           ##inv se establece en NULL, inicializ�ndolo como un objeto dentro del entorno makeCacheMatrix ()
  set1 <- function(y) { ##y es el argumento de la funci�n set1 y es una matriz.
    x <<- y             ##Asigna el argumento de entrada al objeto x en el entorno principal y
    inv <<- NULL        ##Esta l�nea de c�digo borra cualquier valor inv que haya sido almacenado en cach� por una ejecuci�n previa de cacheSolve().
  }
  get1 <- function() x                       ##get1 es una funci�n que capta el valor de la matriz x
  setsolve <- function(solve) inv <<- solve  ## se utiliza el operador de doble asignaci�n para asignar el argumento de entrada al valor de la inversa inv en el entorno principal.
  getsolve <- function() inv                 ##getsolve es una funci�n que capta el valor de la inversa inv
  list(set1 = set1, get1 = get1,             ##Las funciones set1, get1, setsolve, getsolve se asignan como elementos de una lista,  y lo devuelve al entorno padre. 
       setsolve = setsolve,
       getsolve = getsolve)
}

## La funci�n cacheSolve calcula la inversa de la "matriz" especial devuelta porla funci�n makeCacheMatrix.
## Si ya se ha calculado la inversa (y la matriz no ha cambiado), entonces la soluci�n de cach� deber�a recuperar la inversa de la cach�.

cacheSolve <- function(x, ...) {         ## La funci�n cacheSolve toma el argumento x, y el argumento ... que pasar argumentos adicionales a la funci�n.
  inv <- x$getsolve()                    ## La funci�n intenta recuperar la inversa inv del objecto pasado como argumento. 
  if(!is.null(inv)) {                    ## comprueba si el resultado no es NULL.
    message("obteniendo datos en cach�") ## De ser as�, arroj� el mensaje "obteniendo datos en cach�".
    return(inv)                          ## Devuelve al entorno principal.
  }
  data <- x$get1()        ##Si el resultado es NULL, obtenemos la matriz del objeto de entrada. Y la guardamos en la variable data.
  inv <- solve(data, ...) ## Calculamos la inversa. 
  x$setsolve(inv)         ##Establece la inversa del objeto de entrada.
  inv                      ## Devuelve el valor de la inversa de 'x' al entorno principal imprimiendo el objeto de la inversa.
}