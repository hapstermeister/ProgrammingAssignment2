## WEEK 3 Assignment
##
## Assumptions:
## For this assignment, assume that the matrix supplied is always invertible
##

# Function:    makeCacheMatrix
# Description: Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   # Set the matrix
   set <- function (y) {
      x <<- y
      inv <<- NULL
   }
   # Get the matrix
   get <- function() x
   # Set the inverse matrix
   setInverse <- function(inverse) inv <<- inverse 
   getInverse <- function() inv
   
   # Create special "matrix" object that has a cache for its inverse
   list (set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


# Function:    cacheSolve
# Description: Computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the # inverse has already been calculated (and the
# matrix has not changed), then the cacheSolve should retrive the inverse from
# the cache.

cacheSolve <- function(x, ...) {
   # Get cached inverse from special "matrix" x
   inv <- x$getInverse()
   if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   # Get the matrix data from special "matrix" x
   data <- x$get()
   # Solve for the inverse of Set inverse
   inv <- solve(data)
   x$setInverse(inv)
   inv
}
