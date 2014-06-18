## Create a cacheMatrix object for an invertable matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  #Initialize the variable holding the cached inverse matrix
  cachedInverse <- NULL
  
  # sets the value of matrix to y using the superassignment operator 
  # from other environment in the search() list
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  
  
  get <- function() x
  # get the value of x
  
  setInverse <- function(inverse) cachedInverse <<- inverse
  
  getInverse <- function() cachedInverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return the inverse of an cacheMatrix object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invFunc <- x$getInverse()
  
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
}
