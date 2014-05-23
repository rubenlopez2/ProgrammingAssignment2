## makeCacheMatrix makes a list of functions that work with the
## Matrix.

makeCacheMatrix <- function(x = matrix()) {
  ## Set the value of the inverse matrix to null.
  
  inv <- NULL
  
  ## Creates the function to set a matrix. the values are saved to
  ## the parent frame.
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Function to get matrix.
  
  get <- function() x
  
  ## Sets the value for the solved inverse of the matrix.
  
  setinv <- function(inverse) inv <- inverse
  
  ## Gets the value of the inverse.
  
  getinv <- function() inv
  
  ## Creates the list of functions.
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function tests to see if there is a previously calculated 
## inverse value has been calculated previously. If it has not, it
## will calculate one.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  ## calls the function to get the value of the inverse
  
  inv <- x$getinv()
  
  ## If inv is not null, then the condition will be true and will 
  ## display a message indicating that it is using cached data
  ## and then display the data and exit the function.
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Pulls the matrix to perform the calculations.
  
  data <- x$get()
  
  ## Calculates the inverse of the square matrix.
  
  inv <- solve(data,...)
  
  ## Calls the function to set the value of the inverse of the matrix.
  
  x$setinv(inv)
  inv
}
