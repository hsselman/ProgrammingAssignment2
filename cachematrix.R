setwd("D:/Data/hse21048/Documents/Coursera/R Programming/ProgrammingAssignment2")

## This file contains two functions.

## makeCacheMatrix defines a list of 4 functions based on matrix x
## (get, set, getinverse, setinverse) 

## cacheSolve takes a CacheMatrix and returns the inverse (from Cache if 
## available, and otherwise it computes it and stores it for later use)


## Define a list of four functions to tranform a matrix into a CacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Take a CacheMatrix (defined with makeCacheMatrix) and return it's inverse
## If available the inverse is returned from memory.

cacheSolve <- function(CacheMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- CacheMatrix$getinverse()
        if(!is.null(inv)) {
                message("getting inverse from cache")
                return(inv)
        }
        data <- CacheMatrix$get()
        if(nrow(data)==ncol(data)){
                inv <- solve(data, ...)
                CacheMatrix$setinverse(inv)
                inv
        }else{
                message("not a square matrix")
        }
}
