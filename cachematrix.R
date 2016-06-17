## Taking the inverse of a matrix is typically a fast operation.
## However, for repeated matrix inversions, it may be more efficient
## to store the cache the inverse, so that it can be looked up in the cache
## rather than recomputed. Therefore, below are a set of functions that
## can create a special object that stores a matrix and caches its inverse.

## The makeCacheMatrix function creates a special object that stores a
## matrix and can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The cacheSolve function can either compute the inverse of the special
## object made above, or return the cached inverse if it has already been
## computed.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
