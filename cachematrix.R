## Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly (there are also alternatives to 
## matrix inversion that we will not discuss here). This  pair of 
## functions cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

library(MASS)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    if (nrow(data) == ncol(data)){  #if it´s square matrix
        i <- solve(data, ...)
    }else{
        i <- ginv(data)
    }
    x$setinverse(i)
    i
}
