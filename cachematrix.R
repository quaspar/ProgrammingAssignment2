## Functions for caching, storing and retrieving the inverse of a matrix 
## so that it doesn't have to be computed multiple times


## Function makeCacheMatrix takes a matrix as parameter and can store the inverse of that matrix.
## makeCacheMatrix returns a list populated with setter and getter functions.
## The matrix can be reset with the "set" function.
## The inverse can be stored with the "setinverse" function.
## The values can be retrieved with the "get" and "getinverse" functions.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cacheSolve takes our "special kind of matrix" (the makeCacheMatrix object) as parameter
## and returns the inverse of the matrix. 
## First, it checks for a cached value. If there is none, the inverse is computed and then cached.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## If there was no cached data, we have to compute the inverse:
  message("computing ...")
  data <- x$get()
  inverse <- solve(data, ...)
  ## Caching the inverse for future calls:
  x$setinverse(inverse)
  ## Returning the newly computed inverse:
  inverse
}
