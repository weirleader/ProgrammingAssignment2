## As described in Programming Assignment 2 of the "R Programming" course,
## this program consists of two functions that will allow for the setting
## of a matrix, the computation of its inverse, and the caching of that
## inverse to conserve resources since the solution of a matrix (specifically,
## a big one) can be resource-intensive



## makeCacheMatrix() creates a special "matrix" object that can cache its
## inverse; more specifically, it returns a list containing four functions,
## based upon an initial provided matrix.  Those functions are:
##   set()    -- stores the provided matrix AND initializes the inverse to NULL
##   get()    -- simply returns the currently-stored matrix
##   setinv() -- computes and caches the matrix inverse (this should only be
##               called from within the 'cacheSolve' function, as otherwise
##               it bypasses the functional mechanism that ensures we store
##               meaningful results).
##   getinv() -- simply returns the cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(mat_inv) inv <<- mat_inv
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## cacheSolve() is passed a matrix and performs one of two actions:
##   (1) if the inverse has already been computed/cached, it displays a
##       message to that effect and then returns the cached inverse
##   (2) if the inverse has NOT been compute/cached, this function will
##       compute the inverse, cache it, and return the inverse

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)){
            message("getting cached inverse")
            return(inv)
      }
      curr_matrix <- x$get()
      inv <- solve(curr_matrix, ...)
      x$setinv(inv)
      inv
}
