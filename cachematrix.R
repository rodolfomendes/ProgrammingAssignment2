## Put comments here that give an overall description of what your
## functions do

## Create a matrix object which allows caching
##  its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y){
        x <<- y
        inverse <- NULL # discard cache if value changes
    }
    
    get <- function() x
    
    setInverse <- function(inv){
        inverse <<- inv
    }
    
    getInverse <- function() inverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calculates and cache the inverse matrix of a cacheable
##  matrix objects; The computation is performed only if
##  there's no cached result

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    if(!is.null(inv)){
        message('Getting cached inverse matrix.')
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
}
