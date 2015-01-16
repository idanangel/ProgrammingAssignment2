## The functions below inverse a square matrix. 
## To avoid recalculating the matrix inverse, the cached matrix would be returened 
## in case the original matrix hasn't changed and that the matrix inverse was already calculated

## This funcion will create a vector containing 4 functions to get/set the matrix and 
## also to get/set the inverse of the matrix. The list of functions retured by makeCacheMatrix
## can be used t created a special "matrix" that can cache it's own invers
makeCacheMatrix <- function(x = matrix()) {
     
     ## cachedInverse is initialized to null
     cachedInverse <- NULL
     
     ## use the setMatrix function to store a matrix 
     setMatrix <- function(y) {
          x <<- y
          cachedInverse <<- NULL
     }
     
     ## ues the getMatrix function to get a previously stored matrix
     getMatrix <- function() x
     
     ## use this function to set a new calculated matrix inverse
     setInverse <- function(inverse) cachedInverse <<- inverse
     
     ## use this function to get the cached matrix inverse
     getInverse <- function() m
     
     ## List that allows acceess to all the functions for cacheing matrix and it's inverse
     list(setMatrix = setMatrix, getMatrix = getMatrix,
          setInverse = setInverse,
          getInverse = getInverse)
}


## This function returns the inverse of the special cache matrix that's passed to it.
## In case the Inverse was already calculated before, the cached inverse would
## be returned and inverse will not be calculated again
cacheSolve <- function(x, ...) {
     
     ## Check if the inverse matrix was already calculated, if so, return it
     inverse <- x$getInverse()
     if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
     }
     
     ## Inverse was not calculated yet for this matrix- Calculate Inverse:
     
     ## Get the matrix
     mat <- x$getMatrix()
     
     ## Calculate the inverse
     inverse <- solve(mat, ...)
     x$setInverse(inverse)
     
     return (inverse)
}
