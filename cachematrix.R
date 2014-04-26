## This file contains two functions used to store
## a numeric matrix and cache its inverse.

## makeCacheMatrix creates a list of functions:
##     set: sets the value of the matrix
##     get: gets the value of the matrix
##     setinverse: sets the value of the inverse
##     getinverse: gets the value of the inverse

## cacheSolve obtains the inverse of the input 
## matrix from the cache if it has already been 
## calculated, or it calculates and caches it.

## Example input:
##     b <- matrix(1:4,2)
##     a <- makeCacheMatrix(b) 
##     cacheSolve(a)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(solve) m <<- solve
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
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
