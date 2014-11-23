## Put comments here that give an overall description of what your
## functions do
##
## The makeCacheMatrix function creates an object containing getters and setters for
## a matrix that is saved in the object and also getters and setters for the inverse
## of that matrix. It returns a list of functions that can be used and due to
## lexical scoping the values of variables set during creation are kept and protected
## inside the CacheMatrix object.
##
## The cacheSolve function computes the inverse of the matrix using the solve() function
## of R. Given a CacheMatrix, the function will first check if the inverse was already
## computed and stored in the object. If not it will compute a new inverse and 

## This function creates the inverse caching matrix object
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function test if the inverse is already cached and returns it,
## otherwise is computes the inverse and returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinverse(inv)
  inv
}
