## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cm <- NULL  ## Cache matrix is null initially
  set <- function(y) { ## set function sets the matrix
    x <<- y
    cm <<- NULL
  }
  get <- function() x  ## returns the matrix
  setinvmatrix <- function(inverse) cm <<- inverse 
  getinvmatrix <- function() cm
  list(set = set, get = get,
       setinvmatrix = setmatrix,
       getinvmatrix = getinvmatrix)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  cache <- x$getinvmatrix()             ##checks if inverse already there 
  
  ## returns if not null the cached value
  if(!is.null(cache)) { 
    message("getting cached data")
    return(cache)
  }
  data <- x$get()                       ## if not cached already then gets the matrix
  cache <- solve(data, ...)             ## creates the inverse through solve function
  
  x$setinvmatrix(cache)                 ##sets the cached value to inverse
  
  ## Return a matrix that is the inverse of 'x'
  return(cache)
}

