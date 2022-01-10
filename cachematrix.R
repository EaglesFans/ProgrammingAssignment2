## Caching the Inverse of a Matrix
## Week 3 Program Assignment

## Purpose of this assignment is to write a pair of functions that cache the inverse of a matrix.

## MakeCacheMatrix makes a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {m <<- inverse}
  getInverse <- function() {m} ## if there is no cached data, it returns with "NULL".
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## casheSolve Matrix calcuates the inverse of the matrix above.

cacheSolve <- function(x, ...) { 
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data") ## If there is cashed data, the inverse of a matrix will come with the message.
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
