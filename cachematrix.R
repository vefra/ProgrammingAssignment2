## Fecha: 19/02/2017
## Nombre: Verónica Vaca

## R Programming Assigment 2: Caching the inverse of a matrix

## La función makeCacheMatrix crea una matriz especial que 
## es una lista la cual contiene una función que hace:

## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m<- NULL
	set <- function(y) {
		x <<- y
		m <<-NULL
	}
	get<- function() x 
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(
		set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## La funcion cacheSolve calcula la inversa de la matriz 
## especial dada por la funcion anterior verificando previamente
## si la inversa ya fue calculada. Si es asi toma la 
## inversa del cache si no la calcula. (Este codigo es adaptado del
## codigo en coursera dado como ejemplo)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}




