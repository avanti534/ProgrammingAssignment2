# makeCahceMatrix creates a speical matrix with specific
# get and set functions that can be used to cache and store
# the inverse of the specific matrix

# Create a special matrix, which can be used to calculate 
# the inverse and store the inverse (data) in a cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # This is the invere value (NULL at start)
  set <- function(y) { # Save values of our matrix
    x <<- y # Set new values for our matrix
    m <<- NULL # Because we updated the values, we no longer know what the inverse is
  }
  get <- function() x # Return our matrix
  setInverse <- function(inverse) m <<- inverse # Set the inverse to this given value
  getInverse <- function() m # Return the inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) # Possible functions
}


# This will check to see if our special matrix has a cached value.
# If it does : We will get the value from the cache
# Otherwise : We need to calculate the inverse value for the matrix to store in the  cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse() # Get the current value for this special matrix
  if (!is.null(m)) {
    # The inverse is not null and we can return the cached data
    print("Getting cached data")
    return(m)
  }
  
  # We dont have any cached data, so we need to calculate the inverse
  data <- x$get() # Get current (non inverse) matrix
  m <- solve(data, ...) # Solve for inverse
  x$setInverse(m) # Set the inverse
  m # Return...
}
