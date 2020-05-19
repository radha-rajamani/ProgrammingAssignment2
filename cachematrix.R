## R Programming -> Week 3 -> Programming Assignment 2: Lexical Scoping 
## makeCacheMatrix creates a special matrix object that caches its inverse
## It contains 4 different functions - setmat(), getmat() - set and get a matrix
## setinvrs(), getinvrs() - set and get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invrs <- matrix(data = NA) # invrs matrix is initialised to NA
  message("1st initialise:") 
  print(invrs)
  
  setmat <- function(y) {
    x <<- y
    invrs <<- matrix(data = NA) # invrs matrix is initialised to NA
    message("Initialise inside setmat:")
    print(invrs)
  }   
  
  getmat <- function() x
  
  setinvrs <- function(z) invrs <<- z
  
  getinvrs <- function() invrs
  
  list(setmat = setmat, 
       getmat = getmat,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed)
## retrieve the inverse from cache 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invrs <- x$getinvrs()
  message("Output of getinvrs:")
  print(invrs)
  
  if (all(is.na(invrs))) {   # if invrs matrix still has NA, do nothing
  }
  else {                     # if invrs matrix has been set, get from cache
    message("Getting cached data:")
    return(invrs)
  }
  
  mat <- x$getmat()
  invrs <- solve(mat)
  x$setinvrs(invrs)
  invrs
  
}
