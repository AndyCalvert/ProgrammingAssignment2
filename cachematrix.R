## Matrix inversion is usually a costly computation. These functions support caching of
## an inversion operation.

## This function creates a special 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## 'i' is the cached inverse.
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL    
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function computes the inverse of the special 'maxtrix' returned by 
## makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed, 
## then this function retrieves the inverse from the cache rather than 
## calculating it.

cacheSolve <- function(x, ...) {
  
  ## Do we already have a cached answer ?
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    
    ## return that cached inverse
    return(i)
  }
  
  ## No we don't, so calculate one, and store it for possible later use.
  ## Note that for this assignment we are told to assume it is invertible. 
  ## Normally there should be some significant error trapping included
  ## (matrix not square, matrix not invetible, etc)
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  ## and simply return the inverse.
  i
}
