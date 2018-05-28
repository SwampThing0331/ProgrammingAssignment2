## These functions work together to reduce the time it takes to calculate
## the inverse of a matrix.

## The first function creates a vector containing a function to set the
## vector's value, get its value, set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      seti <- function(solve) i <<- solve
      geti <- function() i
      list(set = set, get = get
           ,seti = seti
           ,geti = geti)
}

## The second function calculates the inverse of the matrix only after it
## checks to see if the inverse was not previously cached. If it was, the 
## cached value is returned. If it was not previously cached, the 
## function calculates the inverse and then caches it for future use.

cacheSolve <- function(x, ...) {
      i <- x$geti()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$seti(i)
      i
}
