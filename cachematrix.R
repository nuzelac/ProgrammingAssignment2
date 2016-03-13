# Following functions can be used to create a special kind of matrix
# that caches the result of its inverse instead of computing it repeatedly.

# Creates a special "matrix", which contains functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# Calculates the inverse of the matrix and caches
# it for subseqeuent calls on the same matrix.
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
