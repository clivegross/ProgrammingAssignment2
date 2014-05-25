# cachematrix.R
#
# A pair of functions used to cache the inverse of a matrix so as to conserve
# computational demand on repeated operations.
# 
# Usage:
# makeCacheMatrix (function): Creates a special "matrix" object that can cache
# its inverse
# cacheSolve (function): Computes the inverse of the special "matrix" object 
# returned by makeCacheMatrix. If the inverse has already been calculated and the
# matrix has not changed, cacheSolve will simply retrieve the inverse from the
# cache instead of recalculating.

# Pass a matrix into makeCacheMatrix (or leave empty for an empty matrix) to
# return a list containing functions:
# set: set the value of the matrix object and empty the cache
# get: return the matrix object
# setinverse: calculate the inverse matrix and update the cache
# getinverse: return the inverse matrix object
makeCacheMatrix <- function(x = matrix()) {
  # initialise the cache
  m <- NULL
  # define the set function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # define the get function
  get <- function() x
  # define the setinverse function
  setinverse <- function(solve) m <<- solve
  # define the getinverse function
  getinverse <- function() m
  # define the list (of functions) object to be returned
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Pass a "matrix" defined by makeCacheMatrix into cacheSolve to return
# the inverse matrix either by calculation (using solve) or if it has
# already been solved, retrieve from the cache
cacheSolve <- function(x, ...) {
  # get the inverse matrix object
  m <- x$getinverse()
  # if inverse matrix already defined (not null), return it from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if inverse matrix not defined (cache is null), calculate it
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m    
}
