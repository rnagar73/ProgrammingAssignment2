## Put comments here that give an overall description of what your
## functions do
#
# Description : Matrix inversion is usually a costly computation and there is 
# benefit to caching the inverse of a matrix rather than compututing it 
# repeatedly 
#
## This function creates a special "matrix" object that can cache its inverse.
## Parameters : Matrix
## Returns list of following functions
#    setval - sets the value of the matrix
#    getval - the value of the matrix
#    setinv - the value of inverse of the matrix
#    getinv - gets the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setval <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getval <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(setval=setval, getval=getval, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
# The below function computes the inverse of the matrix return by makeCacheMatrix. It checks if
# the inverse already exists If it does it returns cached inverse value else it will 
# calculate inverse using setinv function and return the value. 
# It will also cache the value for future use.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting from cache.")
        return(i)
    }
    val <- x$getval()
    i <- solve(val)
    x$setinv(i)
    i
}
