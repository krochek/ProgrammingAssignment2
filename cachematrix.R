
## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix that supports caching the inverse 
## the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function recieves an object from the type of makeCacheMatrix and
## checks if the inverse is cached. If it is cached, the function will notify
## this through a message and return the inverted matrix. If not, it will 
## cache the Inverted matrix and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
