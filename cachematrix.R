## This functions allow establishing a cache of a matrix and its inverse.
## 'makeCacheMatrix' creates the cache, and 'cacheSolve' obtains the value of
## the inverse, or sets its value int he cache.

## This function caches the Matrix 'x' into it's enviroment, and provides
## subfunctions to get or change either it's value or it's inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    setMatrix <- function(y) {
        x <<- y
        i <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(Inv) i <<- Inv
    getInverse <- function() i
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function takes the list produced as output of the funtion 
## makeCacheMatrix and gets the value of the inverse from the cache, or sets
## its value.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        return(i)
    }
    Matrix <- x$getMatrix()
    i <- solve(Matrix, ...)
    x$setInverse(i)
    i
}
