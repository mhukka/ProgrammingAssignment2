# The following functions will create and compute the inverse of a cached special inverse matrix object.

# The first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
# m is initialized as an object within the environment to be used later to code the function.
# Assigning NULL clears any cached value for m
  inv <- NULL
}

## Following modules retrieve and set data values within an object that is being created.

set <- function(y) {
  
  ## Assign the input argument to the x in the parent environment.
  x <<- y
  inv <<- NULL
}

## Define the getter for the vector x that retrieves x from the parent environment

get <- function() x

## Define setter for the matrix

setinverse <- function(inverse) inv <<- inverse

## Define getter for the matrix

getinverse <- function() inv

# Assign each function an element within a list; returns it to the parent environment.

list(set = set,          # gives the name 'set' to the set() function defined above
     get = get,          # gives the name 'get' to the get() function defined above
     setinverse = setinverse,  # gives the name 'setinverse' to the setinverse() function defined above
     getinverse = getinverse)  # gives the name 'getinverse' to the getinverse() function defined above

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
# Return an inverse matrix of 'x'
  inv <- x$getInverse()
  
# Return the inverse matrix, if its already set
  if( !is.null(inv) ) {
    message("getting cached data")
    return(inv)
  }
  
# Get the matrix from the object
  data <- x$get()
  
# Calculate the inverse using matrix multiplication
  inv <- solve(data) %*% data
  
# Set the inverse to the object
  x$setInverse(inv)
  
# Return the matrix
  inv
}