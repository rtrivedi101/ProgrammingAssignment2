
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inva <- NULL
  set <- function(y) {
    x <<- y
    inva <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inva <<- inverse
  getInverse <- function() inva
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inva <- x$getInverse()
  if (!is.null(inva)) {
    message("getting cached data")
    return(inva)
  }
  matr <- x$get()
  inva <- solve(matr, ...)
  x$setInverse(inva)
  inva
}