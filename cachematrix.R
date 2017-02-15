## Cache the inverse of a matrix
## Since matrix inversion is a costly operation the following two functions 
## makeCacheMatrix and cacheSolve would help us to create a matrix and cache
## the inverse of the matrix. If the same matrix is passed again for 
## inversion the cached value would be returned

## The following function is supposed to store a matrix and cache the 
## inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
  
  cacheInverse <- NULL
  
  set <- function(newMatrix)
  {
    x<<-newMatrix
    cacheInverse <-NULL
  }
  
  get <- function()
  {
    x
  }
  
  setInverse <- function(inverse)
  {
    cacheInverse <<-inverse
  }
  
  getInverse <- function()
  {
    cacheInverse
  }
  
  list(set= set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The following function calculates the inverse of a matrix using the 
## function makeCacheMatrix. The idea is of the inverse is already 
## calculated then it should fetch the data from cahced value. Otherwise 
## calculate the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cacheInverse <- x$getInverse()
  
  if(!is.null(cacheInverse)) {
    message("getting cached data")
    return(cacheInverse)
  }
  
  data <- x$get()
  cacheInverse <- solve(data,...)
  x$setInverse(cacheInverse)
  cacheInverse
}
