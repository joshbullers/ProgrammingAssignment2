## These functions make it possible to return the inverse of a matrix.
## Once inversed, it is stored in cache.

## makeCacheMatrix creates a special list of functions for use in cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <- NULL
    }
    
    get <- function() x
    
    setInverse <- function(solve) inv <<- solve
    
    getInverse <- function() inv
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve checks if inv already exists and returns it if so.
## If it does not exist, it creates the inverse of the matrix,
## stores it in cache and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}