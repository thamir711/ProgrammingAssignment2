## These two functions computes and caches the inverse of a matrix rather than
## computes it repeatedly.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    x.inv <- NULL    
    
    set <- function(y) {
        x <<- y
        x.inv <<- NULL
    }
    
    get <- function() x
    
    set.inverse <- function(inv) x.inv <<- inv
    
    get.inverse <- function() x.inv
    
    list(set = set, get = get, set.inverse = set.inverse, get.inverse = get.inverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat.inv <- x$get.inverse()
    
    if(!is.null(mat.inv)) {
        message("getting cached inverse matrix")
        return(mat.inv)
    }
    
    mat <- x$get()
    mat.inv <- solve(mat, ...)
    x$set.inverse(mat.inv)
    mat.inv    
}
