# makeCacheMatrix creates a list containing a set of generic functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(cacheval) inv <<- cacheval
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data - inverse of the matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Sanple run
## x <- matrix(1:4, nrow = 2, ncol = 2)
## m <- makeCacheMatrix(x)
## Check with m$get()

## cacheSolve(m)
## Check - No cache in the first run
## cacheSolve(m)
## Check - Cache in the 2nd run onwards for same x