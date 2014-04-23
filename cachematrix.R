# Cache Matrix
# 
# Written by Mike Silva for the Coursera R Programming course
#
# These functions take a matrix and finds the inverse
#
################################################################################
# EXAMPLE
# > m <- matrix(c(1, 1, 1, 3, 4, 3, 3, 3, 4),3)
# > m 
# [,1] [,2] [,3]
# [1,]    1    3    3
# [2,]    1    4    3
# [3,]    1    3    4
#
# >cache.matrix <- makeCacheMatrix(m) 
# >m.inverse <- cacheSolve(cache.matrix)
# >m.inverse
# [,1] [,2] [,3]
# [1,]    7   -3   -3
# [2,]   -1    1    0
# [3,]   -1    0    1
# 

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(input.matrix = matrix()) {
  # Initialize the inverse matrix
  inverse.matrix <- NULL
  
  # This function sets value of the matrix
  set <- function(val) {
    input.matrix <<- val
    inverse.matrix <<- NULL
  }
  
  # This function gets the value of the matrix
  get <- function() input.matrix
  
  # This function sets the value of the inverse matrix
  setinverse <- function(cache.matrix) inverse.matrix <<- cache.matrix
  
  # This function gets the value of the inverse matrix
  getinverse <- function() inverse.matrix
  
  # This is what the function returns
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse from 
# the cache.

cacheSolve <- function(makeCacheMatrix.list, ...) {
  # Get the inverse from the cached function that is passed in
  inverse.matrix <- makeCacheMatrix.list$getinverse()
  
  # Check if cached data exists
  if(is.null(inverse.matrix)) {
    # Cached data does not exists so get the matrix
    data <- makeCacheMatrix.list$get()
    
    # Find the inverse using solve method
    inverse.matrix <- solve(data, ...)
    
    # Cache the inverse
    makeCacheMatrix.list$setinverse(inverse.matrix)
  }
  
  # Return the special matrix that is the inverse of 'x'
  inverse.matrix
}