## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 
## we will write a pair of functions that cache the inverse of a matrix.


makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the matrix 
## created with the above function.
## it first checks to see if the inverse has already been calculated. If so, 
## it gets the inverse from the cache. Otherwise, it calculates inverse and 
## sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  z <- x$getinverse()
  if (!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinverse(z)
  z
}
