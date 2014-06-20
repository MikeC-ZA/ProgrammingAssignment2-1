## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set=set, get=get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()

## If the inverse has already been calculated (and the matrix has not changed) then the
## cacheSolve function retrieves the inverse from the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

## If the inverse has not yet been calculated then the cacheSolove function determines
## the inverse matrix and saves it to cache
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
}
