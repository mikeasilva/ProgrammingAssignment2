## Cache Matrix
## 
## Written by Mike Silva for the Coursera R Programming course
##
## These functions take a matrix and finds the inverse
##
################################################################################
## EXAMPLE
## > m <- matrix(c(1, 1, 1, 3, 4, 3, 3, 3, 4),3)
## > m 
## [,1] [,2] [,3]
## [1,]    1    3    3
## [2,]    1    4    3
## [3,]    1    3    4
##
## >cache.matrix <- makeCacheMatrix(m) 
## >m.inverse <- cacheSolve(cache.matrix)
## >m.inverse
## [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1
## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## This is the inverse matrix
  m <- NULL
  ## This function sets value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## This function gets the value of the matrix
  get <- function() x
  ## This function sets the value of the inverse matrix
  setinverse <- function(cache.matrix) m <<- cache.matrix
  ## This function gets the value of the inverse matrix
  getinverse <- function() m
  ## This is what the function returns
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
  ## Get the inverse from the cached function that is passed in
  m <- x$getinverse()
  ## Check if cached data exists
  if(is.null(m)) {
    ## Cached data does not exists so get the matrix
    data <- x$get()
    ## Find the inverse using solve
    m <- solve(data, ...)
    ## Cache the inverse
    x$setinverse(m)
  }
  ## Return a matrix that is the inverse of 'x'
  m 
}
