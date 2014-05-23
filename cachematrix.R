## The following functions create a matrix that is capable of caching
## its own inverse after it has been calculated. This is done to avoid 
## duplicating time-consuming operations.


## This function accepts a matrix and adds functions that 
##   (1) get and set the matrix, and 
##   (2) get and set the inverse
## Also, if the matrix is set to a new value, it clears the cache.

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


## This function accepts a makeCacheMatrix object and checks its cache.
## If a cached value exists, it returns that value and avoids the costly
## calculation. Otherwise, it performs the calculation and stores it
## in the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
