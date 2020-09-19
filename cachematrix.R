## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      # set the value of the matrix
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      # get the value of the matrix
      get <- function() x
      # set the value of the inverse
      setsolve <- function(solve) m <<- solve
      # get the value of the inverse
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      # get the data and invert it 
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
