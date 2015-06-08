## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## The next two functions 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## set the value of the vector
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  ## get the value of the vector
  get <- function() x
  
  ## set the value of the inverse
  setinverse <- function(inv) i <<- inv
  
  ## get the value of the inverse
  getinverse <- function() i
  
  ## the list of availble operations
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## 
  m <- x$getinverse()
  
  
  if(!is.null(m)) {
    ## We are lucky the matrix has been prevously calculated
    ## Just return the cached value
    message("getting cached data")
    return(m)
  }
  
  ## We access to the matrix itself
  data <- x$get()
  ## The inverse is calculated (if possible!!)
  i <- solve(data)
  ## The result is sot
  x$setinverse(i)
  i
  
}
