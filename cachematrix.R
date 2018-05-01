## RProgramming Assignment 2 for Peer Review
##  Vidya Balaje

## This function creates special matrix object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 ## `makeVector` creates a special "vector", which is
 ##  really a list containing a function to
 ##  1.  set the value of the vector
 ##  2.  get the value of the vector
 ##  3.  set the value of the mean
 ##  4.  get the value of the mean
  
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinvrs <- function(inverse) invrs <<- inverse
  getinvrs <- function() invrs
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
}


## This function computes inverse of the 
## special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## x is the output of makeCacheMatrix
        ## Returns a matrix that is the inverse of 'x'
  
  invrs <- x$getinvrs()
  
  ## if inverse already exists, retrieve from cache
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  ## else compute inverse of the matrix 
  matrixdata <- x$get()
  invrs <- solve(matrixdata, ...)
  
  ## set value of inverse in cache
  x$setinvrs(invrs)
  invrs
  
}
