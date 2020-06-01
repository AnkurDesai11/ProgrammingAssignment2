## Put comments here that give an overall description of what your
## functions do

## Recieves the matrix that's inverse has to be found
## Defines 4 functions which are used by cacheSolve function to
## Either find the cached value of the inverse of the matrix
## Or find the inverse of the matrix and store it in cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Driver function that conditionally either retrieves cached value of matrix's inverse
## Or calculates and stores it to cache using functions defined in makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  cacheinv <- x$getinv()
  if(!is.null(cacheinv)) {
    message("getting cached data")
    return(cacheinv)
  }
  data <- x$get()
  cacheinv <- solve(data, ...)
  x$setinv(cacheinv)
  cacheinv
}
