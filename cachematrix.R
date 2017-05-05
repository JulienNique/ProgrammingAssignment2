## The pair of functions above cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(X = matrix()) {
  M <- NULL
  set <- function(Y) {
    X <<- Y
    M <<- NULL
  }
  
  get <- function() X
  setInverse <- function(S) M <<- S
  getInverse <- function() M
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(X, ...) {
  M <- X$getInverse()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  M <- X$get()
  S <- solve(M, ...)
  X$setInverse(S)
  S
}
