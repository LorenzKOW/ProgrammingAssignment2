## Write a short comment describing this function:
## This function creates a special "matrix" object that can cache its inverse.

library(MASS) # library(MASS) is used to calculate for non squared as well as square matrices
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function()x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() {
            inver <- ginv(x)  # to obtain inverse of the matrix
            inver%*%x
      }
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}



## Write a short comment describing this function:
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      
      if (!is.null(inv)){
            message("getting cached data")
            return (inv)
      }
      data <- x$get()
      inv <- solve(data,...)
      x$setinv(inv)
      inv
}
