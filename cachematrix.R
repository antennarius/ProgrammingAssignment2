## This is a set of two functions for caching matrix inversions.
## makeCacheMatrix() takes a matrix and makes a list of 
## functions that will invert the matrix and cache the inversion.


## The following function generates a list that contains functions
## required to calculate and cache the inversion of a given matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
        set <- function(y) {
        x <<- y
        inv <<- NULL
      }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function should be passed a list that was created using 
## makeCacheMatrix. It will return the inversion of the matrix 
## originally used to create that list. If the inversion is 
## already cached, it will return the cached version. Otherwise,
## it will calculate the inversion and return it. 
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
