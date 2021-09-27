## Pair of functions that cache the inverse of a matrix
## How to use: Pass the result of a "makeCacheMatrix" function call to "cacheSolve" function

#' Util function that set the matrix and the inverse in an environment
#' @param x an invertible matrix
makeCacheMatrix <- function(x = matrix()) {
  # if x is not a matrix, following "todo error" will be executed
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#' Compute and cache the inverse of a matrix
#' @param x the result of a previous makeCacheMatrix call
#' @param ... additional arguments to pass to solve function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
