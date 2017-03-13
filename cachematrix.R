## This file contains a pair of functions that cache the inverse of a matrix.

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	## Define the functions and the variables in the parent environment, and then return a list of those

	inv <- NULL
  	set <- function(y) {
    		x <<- y
    		inv <<- NULL
  	}
  	get <- function() x
  	setinverse <- function(inverse) inv <<- inverse
  	getinverse <- function() inv
  	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {

        ## Check if the reverse already exists in the cache and either: return it or calculate the reverse,
	## cache it and return it

	m <- x$getinverse()
  	if(!is.null(m)) {
    		message("getting cached data")
    		return(m)
  	}
  	data <- x$get()
  	m <- solve(data, ...)
  	x$setinverse(m)
  	m
}
