makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  ss <- NULL
  set <- function(y) {
    x <<- y
    ss <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) ss <<- solve
  getsolve <- function() ss
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  ss <- x$getsolve()
  if(!is.null(ss)) {
    message("getting inversed matrix")
    return(ss)
  }
  data <- x$get()
  ss <- solve(data, ...)
  x$setsolve(ss)
  ss
}
