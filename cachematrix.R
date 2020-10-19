## The following functions can be used to solve for the inverse of a matrix.
## The <<- is used to cache the inverse matrix so that if it is alread solved for, it can be referenced.
## Storing output within a cache can greatly reduce time in calculations

## Create a matrix and cach

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() {x}
  setinv <- function(inv) {n <<- inv}
  getinv <- function() {n}
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## calculate the inverse of the matrix defined above or return the inverse that is already cached

cacheSolve <- function(x, ...) {
  n <- x$getinv()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinv(n)
  n
}
