## Caching the Inverse of a Matrix


## Creates a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  e <- NULL
  set <- function(y) {
    x <<- y
    e <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) e <<- inverse
  getinverse <- function() e
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
  e <- x$getinverse()
  if(!is.null(e)) {
    message("getting cached data")
    return(e)
  }
  data <- x$get()
  e <- solve(data, ...)
  x$setinverse(e)
  e
}