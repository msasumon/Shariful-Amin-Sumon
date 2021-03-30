## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(Inverse) {inv <<- Inverse}
  getInverse <- function() {inv}
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}
## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
    mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
}
# data <- matrix(c(1, 1, 4, 0, 3, 1, 4, 4, 0), nrow=3, ncol=3)