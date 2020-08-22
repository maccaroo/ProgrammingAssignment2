## Implementation of a cache for matrix inversion

## A function which caches a matrix and its inverse
makeCacheMatrix <- function(mat = matrix()) {
  
  # Initialise inverse
  inv <- NULL
  
  set <- function(matData) {
    mat <<- matData
    inv <- NULL
  }
  get <- function() mat
  
  setInverse <- function(invData) inv <<- invData
  getInverse <- function() inv
  
  # Return functions
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## A function which calculates the inverse of a matrix and caches the result
## The passed matrix must be invertible
cacheSolve <- function(cacheMat, ...) {
  
  # Get cached inverse if available
  inv <- cacheMat$getInverse()
  if (!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  
  # Otherwise, calculate inverse and cache it
  mat <- cacheMat$get()
  inv <- solve(mat, ...)
  cacheMat.setInverse(inv)
  
  # Return
  inv
}
