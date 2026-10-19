## Put comments here that give an overall description of what your
## functions do

## These functions cache the inverse of a matrix to avoid
## recalculating it when the matrix has not changed.

## Creates a special matrix object that stores the matrix
## and its inverse, and provides functions to get and set them.

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL

  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }

  get <- function() x

  setinverse <- function(solve) {
    inverse <<- solve
  }

  getinverse <- function() inverse

  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Calculates the inverse of a matrix if it is not cached.
## Otherwise, returns the previously cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inverse <- x$getinverse()

  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }

  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)

  inverse
}
