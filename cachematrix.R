## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a wrapper list-object that
## provides support functions to set and get the orignial matrix
## and to set and get the result of the inverse computations.


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve checks if a pre-computed inverse is
## available and returns this in case it is in fact available.
## otherwise the inverse is computed and stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
