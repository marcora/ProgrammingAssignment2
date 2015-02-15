# The function `makeCacheMatrix` takes a matrix as an argument and returns a 
# special "matrix" object that caches the results of potentially time-consuming 
# computations applied to the matrix, for example the function `solve` 
# which returns the inverse of the matrix. You must use the companion function
# `cacheSolve` instead of `solve` with this special "matrix" object to take
# advantage of its time-saving caching behavior.


# This function creates a special "matrix" object that can cache its inverse. 
# This special "matrix" object is really a list containing four functions to:
#   1. set the value of the matrix
#   2. get the value of the matrix
#   3. set the value of the inverse
#   4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(val) inv <<- val
  getinv <- function() inv
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


# This function computes the inverse of the special "matrix" returned by 
# `makeCacheMatrix` above. If the inverse has already been calculated (and the 
# matrix has not changed), then `cacheSolve` should retrieve the inverse from 
# the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
