## Inverse of Matrix Cache
## This following functions are used to store and cache
## the inverse of a matrix for easy use
## The first function creates a list with a function that
## sets the value of the matrix, gets the value of the matrix
## sets the value of the inverse matrix, gets value of inverse matrix


makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) inv <<- inverse
     getInverse <- function() inv
     list(set = set, get = get, setInverse = setInverse,
          getInverse = getInverse)
}

## This next function first checks to see if the inverse of a matrix
## has already computed, and returns the value if so. If not,
## it computes the inverse, and sets the value in the cache

cachemean <- function(x, ...) {
     inv <- x$getInverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     matr <- x$get()
     inv <- solve(matr, ...)
     x$setInverse(inv)
     inv
        ## Return a matrix that is the inverse of 'x'
}
