
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL

  set <- function(y) {
     x <<- y 
     inverse_x <<- NULL
  }

  get <- function() {
     return(x)
  }

  setinverse <- function(inverse) {
     inverse_x <<- inverse   
  }
  getinverse <- function() {
     return(inverse_x)
  }

  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inverse_x <- x$getinverse()
   if (!is.null(inverse_x)) {
      return(inverse_x)
   }
   x <- x$get()
   inverse_x <- solve(x, ...)
   x$setinverse(inverse_x)
   return(inverse_x)
}
