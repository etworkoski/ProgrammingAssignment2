## Title: Programming Assignment 2 - Week 3
## Author: Ellen Tworkoski
## Description: Uses the Solve function to calculate the inverse of a specified
## matrix and caches the result. If the inverse calculation is later requested
## for the same matrix, program retrieves the cached data instead of repeating
## the calculation.
## Notes: Program assumes that the matrix supplied is always invertible.


## The makeCacheMatrix function creates a special "matrix" object that
## can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                                       # initializes inv variable to null
    set <- function(y) {  
        x <<- y                                       # assigns the input matrix to variable in the parent environment
        inv <<- NULL                                  # assigns the inv variable to variable in the parent environment
    }
    get <- function(){x}                              # function that returns matrix
    setinverse <- function(inverse){inv <<- inverse}  # function that stores inverse matrix in parent environment
    getinverse <- function(){inv}                     # function that returns inverse matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned
## by the `makeCacheMatrix`function above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve will
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()   #retrieves value of inverse from parent environment, if it exists
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)         #if inverse already exists in parent environment (cache) then return cached value
    }
    data <- x$get()         #retrieve input matrix
    inv <- solve(data, ...) #calculate inverse of input matrix
    x$setinverse(inv)       #set value of inverse in parent environment for future retrieval
    inv                     #return matrix inverse
}