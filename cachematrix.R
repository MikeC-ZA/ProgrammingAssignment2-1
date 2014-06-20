## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

## Defines a function to set the values in a matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
## Defines a function to get the values in a matrix
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set=set, get=get,

## This line sets the inverse of the matrix
       setInverse = setInverse,

## This line gets the inverse of the matrix
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
