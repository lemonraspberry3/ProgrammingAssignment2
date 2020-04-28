# This script contains code for two functions:
# makeCacheMatrix and cacheSolve. These functions
# take a matrix and cache the inverse of that 
# matrix. The inverted matrix can then be 
# retrieved from the cache, rather than recalculating 
# the inverse of the matrix each time.

# The first function is makeCacheMatrix. It
# takes a matrix and assigns to it a list
# of 4 functions (set, get, setinverse, and
# getinverse) and the variables x and inv, 
# which are used in the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# In cases where the inverted matrix is already
# stored in the cache, cacheSolve will check,
# see that the inverted matrix exists, and 
# simply return the inverted matrix (inv). If
# no matrix has been stored to the cache yet,
# cacheSolve will calculate the inverted matrix,
# store it in the cache, and return the inverted
# matrix. On subsequent calls of cacheSolve, it
# will then return the previously calculated and
# stored inverted matrix without needing to 
# perform any calculations again. However, if a
# new matrix is set, then cacheSolve will again
# calculate the inverted matrix for the new data
# and store the new values in the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
