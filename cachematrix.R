##The purpose of caching the inverse of a matrix is to avoid the need to compute the inverse repeatedly, especially when the computation is time-consuming.

# Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse cache
  
  # Set the matrix
  set <- function(y) {
    x <<- y  # Assign to the global environment
    inv <<- NULL  # Reset the inverse cache
  }
  
  # Retrieve the matrix
  get <- function() {
    x  # Return the matrix
  }
  
  # Retrieve the cached inverse
  getInverse <- function() {
    inv  # Return the inverse cache
  }
  
  # Compute and cache the matrix inverse
  setInverse <- function(inverse) {
    inv <<- inverse  # Cache the inverse
  }
  
  # Return a list of functions
  list(set = set, 
       get = get, 
       getInverse = getInverse, 
       setInverse = setInverse)
}

# Compute the inverse of the matrix, either by calculating it or retrieving it from the cache if it has been previously calculated
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if the inverse is already cached
  
  # If the inverse is not cached, compute it and cache it
  if (!is.null(inv)) {
    message("Retrieving cached inverse")
    return(inv)
  } else {
    message("Calculating inverse and caching result")
    data <- x$get()
    inv <- solve(data, ...)  # Compute the inverse
    x$setInverse(inv)  # Cache the inverse
    return(inv)  # Return the inverse
  }
}
