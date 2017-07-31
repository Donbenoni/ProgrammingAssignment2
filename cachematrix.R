## The below function is used to cache the inverse of a matrix
## Matrix inversion can be costly and timely computation
## therefore there is a benefit to cache the inverse to help save time

## The below function creates a matrix that can store/cache the inverse results.

makeCacheMatrix <- function(x = matrix()) {
  ##set x = to the matrix function
  inv <- NULL
  set <- function(y) {
  ## set function within the makeCacheMatrix to use << from parent environment
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  ## return the x matrix
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## The below function returns the inverse of the above set matrix.
## if the inverse of the matrix has already been created, the inversve matrix should be
## retrieved from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

