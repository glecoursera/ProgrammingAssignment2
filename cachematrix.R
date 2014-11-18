## The following functions create a special matrix object that can cache
## its inverse and then uses that cached inverse later instead of 
## recalculating it again.

## makeCacheMatrix creates the special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    # When the matrix is set the inverse is set to NULL to indicate
    # the matrix has changed and the inverse must be recalculated.
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
    
}


## cacheSolve returns a matrix that is the inverse of 'x'.  The cached
## inverse is used if present.  If the inverse of 'x' does not
## exist the inverse is calculated and added to the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return (inv)
  }
  
  data <-x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
