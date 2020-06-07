## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function creates a special "matrix" and helps find the invert of that matrix using the R function solve()
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function() inv <<- solve(x)
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
##this fucntion checks if the matrix is inverted if it is it returns the inverted matrix
##the cachesolve function computes the inverse of the cached matrix which is returned by the makecachematrix function,
##if the inverse has already been calculated and the matrix hasnt changed then the cache solve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
##we use here the function 'solve' to produce the inverted matrix.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv()
  inv
}