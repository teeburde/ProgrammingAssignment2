## Referencing the inital makeVector format 
## combined with the cache. We will use "inverse" in place
## of "mean" for the both function, as it follows similar
## framework.

## The following function creates a specific Matrix that
## can cache its inverse, per input matrix (x).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  #m for matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvm = function(invm) m <<- invm
  getinvm = function() m
  list(set = set, get = get,
       setinvm = setinvm,
       getinvm = getinvm)
}


## This function determines the inverse of matrix (x) derived from
## the above equation and stores it in the cache.

cacheSolve <- function(x, ...) {
  m = x$getinvm()   #x in terms of inverse matrix
  if(!is.null(m)) { #if done will instantly return, if not will move on to get data
    message("getting cached data")
    return(m)
    
  }
  
  data = x$get()
  m = solve(data, ...)
  x$setinvm(m)
  
}
