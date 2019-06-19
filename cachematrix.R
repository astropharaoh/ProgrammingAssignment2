## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.


## The function below returns a list of functions:
## set() to set the values of a matrix
## get() to return the values of a matrix
## setinv() to set the values of the inverse of the matrix defined above
## getinv() to return the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  invs <- NULL								
  set <- function(y) {									              # sets values of matrix
    x <<- y
    invs <<- NULL
  }
  get <- function() x									                # retrieves matrix
  
  setInverse <- function(inverse) invs <<- inverse		# sets values of inverse
  getInverse <- function() invs							          # retrieves inverse
  
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  invs <- x$getInverse()								# retrieves inverse
  
  if (!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  
  mat <- x$get()
  invs <- solve(mat, ...)								# calculates inverse
  x$setInverse(invs)									  # caches inverse
  invs
}