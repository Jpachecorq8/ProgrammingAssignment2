## ====================================================================
## - Assignment: Caching the Inverse of a Matrix
## - Background:
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly
## - Purpose:
## The functions below contains the logic required to create an object that stores
## a matrix and caches its reverse
## ====================================================================


## ====================================================================
## - Name: makeCacheMatrix
## - Purpose: 
## This function creates a special "matrix" object that can cache its inverse.
## ====================================================================
makeCacheMatrix <- function(x = matrix()) {
    ## Initializes the inverse object
    inv <- NULL
    ## Set the value of the matrix
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## Get the value of the matrix
    getMatrix <- function() x
    ## Set the value of the inversed matrix
    setInversedMatrix <- function(inverse) inv <<- inverse
    ## Get the value of the inversed matrix
    getInversedMatrix <- function() inv
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInversedMatrix = setInversedMatrix,
         getInversedMatrix = getInversedMatrix)
}

## ====================================================================
## - Name: cacheSolve
## - Purpose: 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## ====================================================================
cacheSolve <- function(x, ...) {
    ## Tries to get cached data for the provided matrix
    inv <- x$getInversedMatrix()
    ## Validates if the inverse of the matrix was already cached
    if (!is.null(inv)) {
      ## Return a cached matrix that is the inverse of 'x'
      message("Getting cached matrix <inverse>")
      return(inv)
    }
    ## If the inverse has not been cached, then the function calculates it
    data <- x$getMatrix()
    inv <- solve(data, ...)
    x$setInversedMatrix(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}

## ====================================================================
## - Unit Testing (Including Output)
## ====================================================================
## > source("ProgrammingAssignment2/cachematrix.R")
## > my_matrix <- makeCacheMatrix(matrix(3:6,2,2))
## > my_matrix$getMatrix()
##      [,1] [,2]
## [1,]    3    5
## [2,]    4    6
## > my_matrix$getInversedMatrix()
## NULL
## > cacheSolve(my_matrix)
##      [,1] [,2]
## [1,]   -3  2.5
## [2,]    2 -1.5
## > cacheSolve(my_matrix)
## Getting cached matrix <inverse>
##      [,1] [,2]
## [1,]   -3  2.5
## [2,]    2 -1.5
## > my_matrix$getInversedMatrix()
##      [,1] [,2]
## [1,]   -3  2.5
## [2,]    2 -1.5
## > my_matrix$setMatrix(matrix(c(1,2,9,10),2,2))
## > my_matrix$getMatrix()
##      [,1] [,2]
## [1,]    1    9
## [2,]    2   10
## > my_matrix$getInversedMatrix()
## NULL
## > cacheSolve(my_matrix)
##       [,1]   [,2]
## [1,] -1.25  1.125
## [2,]  0.25 -0.125
## > cacheSolve(my_matrix)
## Getting cached matrix <inverse>
##       [,1]   [,2]
## [1,] -1.25  1.125
## [2,]  0.25 -0.125
## > my_matrix$getInversedMatrix()
##       [,1]   [,2]
## [1,] -1.25  1.125
## [2,]  0.25 -0.125