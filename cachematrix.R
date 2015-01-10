## The functions below inverse a square matrix. 
## To avoid recalculating the matrix inverse, the cached matrix would be returened 
## in case the original matrix hasn't changed and that the matrix inverse was already calculated

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     invers <- NULL
     setMatrix <- function(y) {
          x <<- y
          invers <<- NULL
     }
     getMatrix <- function() x
     setInverse <- function(mean) m <<- mean
     getInverse <- function() m
     list(setMatrix = setMatrix, getMatrix = getMatrix,
          setInverse = setInverse,
          getInverse = getInverse)
}


## This function returns the inverse of the matrix that's passed to it
## if the Inverse was already calculated before, the cached inverse would
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
     mat <- x$get()
     
     ## Calculate the inverse
     inverse <- solve(mat, ...)
     x$setInverse(inverse)
     
     return (inverse)
}
