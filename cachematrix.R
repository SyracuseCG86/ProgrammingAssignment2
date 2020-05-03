## Put comments here that give an overall description of what your
## functions do
## The two functions can cache and calculate the inverse of a matrix.  

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.
## I created it by adapting the makeVector() code given earlier to the purpose of inverting a matrix (
makeCacheMatrix <- function(x = matrix) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  SetMatrixInverse <- function(MatrixInversion) m <<- MatrixInversion
  GetMatrixInverse <- function() m
  list(set = set, get = get,
       SetMatrixInverse = SetMatrixInverse,
       GetMatrixInverse = GetMatrixInverse)
}


## Write a short comment describing this function
## This function is meant to invert a matrix via the solve function
## The code is an adaptation from that given by the professor (cacheMean)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$GetMatrixInverse()
        
  ## Return the cached value if it is not null
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## Use the solve function to get the inverse of the matrix 
  m <- solve(data)
  x$SetMatrixInverse(m)
  m
}
