## The functions below able to cache the inverse of a matrix (that it´s 
## invertible).

## How can I use? One example:
## > specialMatrix <- makeCacheMatrix(matrix(c(3, 2, 1, 1), nrow = 2, ncol = 2))
## > cacheSolve(specialMatrix)

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverseOfX = NULL

  set <- function(newMatrix) {
    x <<- newMatrix
    inverseOfX <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(newInverseOfX) inverseOfX <<- newInverseOfX
  
  getInverse <- function() inverseOfX
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseOfX <- x$getInverse()
  
  if(!is.null(inverseOfX)) {
    message("getting cached inversed matrix")
    return(inverseOfX)
  }
  
  matrixOfX <- x$get()
  
  inverseOfX <- solve(matrixOfX,...)
  
  x$setInverse(inverseOfX)
  
  inverseOfX
}
