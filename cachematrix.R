# The following functions will create and compute the inverse of a cached special inverse matrix object.

# The first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
# m is initialized as an object within the environment to be used later to code the function.
# Assigning NULL clears any cached value for m
  m <- NULL
}

## Following modules retrieve and set data values within an object that is being created.

set <- function(y) {
  
  ## Assign the input argument to the x in the parent environment.
  x <<- y
  m <<- NULL
}

## Define the getter for the vector x that retrieves x from the parent environment

get <- function() x

## Define setter for the matrix

setmatrix <- function(matrix) m <<- matrix

## Define getter for the matrix

getmatrix <- function() m

# Assign each function an element within a list; returns it to the parent environment.

list(set = set,          # gives the name 'set' to the set() function defined above
     get = get,          # gives the name 'get' to the get() function defined above
     setmatrix = setmatrix,  # gives the name 'setmatrix' to the setmatrix() function defined above
     getmatrix = getmatrix)  # gives the name 'getmatrix' to the getmatrix() function defined above

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
# Return an inverse matrix of 'x'
  m <- x$getInverse()
  
# Return the inverse matrix, if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
# Get the matrix from the object
  data <- x$get()
  
# Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
# Set the inverse to the object
  x$setInverse(m)
  
# Return the matrix
  m
}