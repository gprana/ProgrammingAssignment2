## Pair of functions that utilizes caching to speed up
## computation of inverse of a matrix

## Creates a matrix object with cacheable inverse
## Functions:
## get() to show the matrix itself
## set() to set the matrix
## setsolve() to set the inverse (used by cacheSolve)
## getsolve() to get the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Returns inverse of a matrix.
## If the inverse hasn't previously been computed,
## compute the inverse and cache it
## Assumption: Matrix given is always invertible
## To test:
## - Create a matrix x using makeCacheMatrix(),
## - Run cacheSolve(x) twice. 
## - The second run should indicate that cached result is used
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
