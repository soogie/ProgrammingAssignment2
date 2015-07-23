## Caching the Inverse of a Matrix
##  makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
##  cacheSolve     : computes the inverse of the special
##
##  These functions assume that the matrix supplied is always invertible.

## makeCacheMatrix
##    This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## cacheSolve
##    This function computes the inverse of the special
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if(!is.null(s)) {
    message("geting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  return(s)
}


