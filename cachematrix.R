## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
    so <- NULL
    
    set <- function(y) {
        x <<- y
        so <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setSolve <- function(inv) {
        so <<- inv
    }
    
    getSolve  <- function() {
        so
    }
    
    list(set = set, get = get, setSolve = setSolve , getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" 
## If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    so <- x$getSolve()
    if(!is.null(so)) {
        message("getting cached data")
        return(so)
    }
    data <- x$get()
    so <- solve(data, ...)
    x$setSolve(so)
    so
}