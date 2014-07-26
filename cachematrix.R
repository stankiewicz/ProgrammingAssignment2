
## this function creates a matrix with additional functions for setting
## and getting inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse<<- NULL
  }
  get <- function() x
  setinverse <- function(val) inverse <<- val
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## this function  returns an inverse of x using matrix cache'ing
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  ## inverting
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
