# makeCacheMatrix creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the cached inverse as NULL
  
  # Set the value of the matrix and reset the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Clear the cached inverse when the matrix changes
  }
  
  # Get the current matrix
  get <- function() x
  
  # Set the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse
  getInverse <- function() inv
  
  # Return a list of functions to interact with the matrix object
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Check if the inverse is already cached
  inv <- x$getInverse()
  
  # If the inverse is cached, return it
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # If not cached, compute the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the computed inverse for future use
  x$setInverse(inv)
  
  # Return the computed inverse
  inv
}

