## Function creates a special matrix object that stores
## its inverse after cacheSolve has been called once.
## cacheSolve must be called at least once to get the index
## directly (via the 'getInverse' method)

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inver) Inv <<- Inver
  getInverse <- function() Inv
  list (set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
  
}


## Function solves for the inverse of the matrix if it
## is not already stored (if the cacheSolve function has
## not been called previously)

cacheSolve <- function(x, ...) {
  Inv <- x$getInverse()
  if(!is.null(Inv)) {
      message ("getting cached data")
      return (Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInverse(Inv)
  Inv
}
