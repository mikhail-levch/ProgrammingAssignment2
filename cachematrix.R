## Functions simular to example in the README.md file.
## The first function creates a special 'matrix' as a list of get/set functions and two variables for matrix and inverse matrix with their initial values.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setM <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getM <- function() x
  setInv <- function(inver) inv <<- inver
  getInv <- function() inv
  list(setM = setM, getM = getM, setInv = setInv, getInv = getInv)
}


## The second function sets the inverse of the matrix and caches it.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getM()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
