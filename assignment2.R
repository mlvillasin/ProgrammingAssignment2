## Programming Assignment 2 - Caching the Inverse of a Matrix
## The followingf functions create a special matrix and caches the inverse

## This function creates a special matrix that can cache its inverse
## x is the dimension of the matrix
## minv is the inverse of the matrix
makeCacheMatrix <- function(m) {

  minv <- NULL
  
  ## sets the matrix to the passed param
  setMatrix <- function(y) {
    m <<- y
    minv <<- NULL
  }
  
  ## returns the matrix
  getMatrix <- function(x) {
    m <- matrix(1:x, ncol=x, nrow=x)
    for (i in 1:x) {
      m[i,] <- sample(100, x)
    }
    m
  }
  
  ## sets the inverse
  setinvMatrix <- function(invm=matrix()) {
    minv <<- invm
  }

  ## gets the inverse
  getinvMatrix <- function() {
    minv
  }

  ## Expose all available functions as return valuex$
  list(setMatrix=setMatrix, getMatrix=getMatrix, setinvMatrix=setinvMatrix, getinvMatrix=getinvMatrix) 
}

## Computes the inverse of the matrix
CacheSolve <- function(x, y, ...) {

  minv <- x$getinvMatrix()
  if (!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  mdata <- x$getMatrix(y)
  minv <- solve(mdata)
  x$setinvMatrix(minv)
  minv
}
 