## This script checks to see if the inverse of a matrix has already been solved and cached.
## If it has, it returns the cache value. If it has not, it sets the cache to the computed
## value and outputs the inverse.

## Defines a group of functions for the makeCacheMatrix object. Sets cache to null each
## time the set function is called.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Checks to see if a cached value exists for m, the inverse of a matrix. If so,
## the cached value is returned. If not, the inverse is computed and set in the
## argument object.

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
