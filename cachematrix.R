## Functions to optimize repeated matrix inverse
## calculation time

## Creates a object containing a list of getter/setter
## functions to access matrix inverse data

makeCacheMatrix <- function(x = matrix()) {
    
  answer <- NULL
  set <- function(y) {
    x <<- y
    answer <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) answer <<- matrix
  getmatrix <- function() answer
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## Takes an object of type makeCacheMatrix and either computes
## the inverse of the matrix or retrieves the cached value

cacheSolve <- function(x, ...) {
  
  answer <- x$getmatrix()
  if(!is.null(answer)) {
    message("getting cached data")
    return(answer)
  }
  data <- x$get()
  answer <- solve(data, ...)
  x$setmatrix(answer)
  answer
}