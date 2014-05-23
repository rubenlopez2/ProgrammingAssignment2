## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## calls the function to get the value of the inverse
  
  inv <- x$getinv()
  ## Tests to see if there is a previously calculated inverse value.
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