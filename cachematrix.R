## This is the 2nd programming assignement for R Programming in
## Coursera.  The following is captured from the assignment with
## additional comments.
##
## Assignment: Caching the Inverse of a Matrix
## This R file contains 2 functions
##
## 1. makeCacheMatrix:  This function creates a special "matrix"
## object that can cache its inverse.  It contains a a list of
## functions.
##    1.a set: set the value of the matrix
##    1.b get: get the value of the matrix
##    1.c setinverse: set the value of the inverse of the matrix
##    1.d getinverse: get the value of the inverse of the matrix
##
## 2. cacheSolve: This function computes the inverse of the
## special "matrix" returned by makeCacheMatrix.  If the inverse
## has already been caculated (and the matrix has not changed)
## , then the cacheSolve shoudl retrieve the inverse from the
## cache.



## makeCacheMatrix function create a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ##InvOfMatrix is the inverse of the matrix
    InvOfMatrix <- NULL
    
    ##set: set the value of the matrix
    set <- function(NewMatrix) {
        x <<- NewMatrix
        InvOfMatrix <<- NULL
    }
    
    ##get: get the value of teh matrix
    get <- function() x
    
    ##setinverse: set the value of the inverse of the matrix
    setinverse <- function(NewInv) {
        InvOfMatrix <<- NewInv
    }
    
    ##getinverse: get the value of the inverse of the matrix
    getinverse <- function() InvOfMatrix
    
    ##construct the object and return the list of function
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special
## "matrix" which is created by makeCacheMatrix.  If the
## inverse is cached then it will return the cached value.
## If not, it will comupte the inverse, cache the value,
## and return the inverse of the matrix.

cacheSolve <- function(x, ...) {
    ##get the inverse by using the getinverse function
    InvOfMatrix <- x$getinverse()
    
    ##return the cached value if the return value is not equal null
    if(!is.null(InvOfMatrix)) {
        message("getting cached data")
        return(InvOfMatrix)
    }
    
    ##if there is no cached data, caculate the inverse of the matrix
    ##, cache the inverse, and return the inverse.
    data <- x$get()
    InvOfMatrix <- solve(data, ...)
    x$setinverse(InvOfMatrix)
    InvOfMatrix
}
