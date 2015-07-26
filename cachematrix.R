## Through a combination fo makeCacheMatrix and cacheSolve matrix inversion is done only if required
## It checks if the inverse exists and creates it only if it doesnt

## makeCacheMatrix caches a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve inverts the matrix only if it cannot already be found in cache

cacheSolve <- function(x, ...) {
    
    ## Try to get the inverse from the cache
    ## If it exists, then return it
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    
    ## If the inverse was not in cache, then compute it
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
