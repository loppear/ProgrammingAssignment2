## These functions encapsulate an invertible matrix with methods to cache
## its inverse in the local scope and to compute and store that inverse
## using the R function solve()

## Creates an 'object' for a matrix with methods for getting/setting the
## matrix and its inverse, and invalidating the inverse if the matrix is re-set.
## Usage:
##   g <- makeCacheMatrix()
##   g$set(m)
##   g$setInverse(i)
##   m <- g$get()
##   i <- g$getInverse()

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Solves the inverse of the matrix contained in the 'makeCacheMatrix'-stored
## object, returning the existing cached inverse if previously stored.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(is.null(i)) {
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
  }
  i
}
