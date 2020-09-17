## makeCacheMatrix is a function that creates a matrix and sets the inverse of matrix.
## cacheSolve will calculate the inverse of matrix created from above function. If value
## of inverse exists already in above cache it will skip computation and get value in
## the cache, else it will compute the inverse and sets its value to the cache.

## This function will create a cache matrix 'x'.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setMatrix <- function(j){
    x <<- j
    i <<- NULL
  }
  getMatrix <- function() x
  setInv <- function(solve) i <<- solve
  getInv <- function() i
  list(set = setMatrix, get = getMatrix,
       setInv = setInv, getInv = getInv)
}


## This function will check whether inverse value exists or not and will 
## compute and return the inverse of matrix 'x'

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if(!is.null(i)){
    message("Getting cached data....")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setInv(i)
  i
}
