# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Variable to store the cached inverse
  
  # Set the matrix value and reset cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cached inverse when the matrix is updated
  }
  
  # Get the matrix value
  get <- function() x
  
  # Set the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse
  getInverse <- function() inv
  
  # Return a list of functions to interact with the matrix object
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute and cache the inverse of the matrix
cacheSolve <- function(x, ...) {
  # Check if the inverse is already cached
  inv <- x$getInverse()
  
  # If the inverse is cached, return it
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the computed inverse
  x$setInverse(inv)
  
  # Return the computed inverse
  inv
}

