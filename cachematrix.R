## Matrix inverse caching

## Create a CacheMatrix with a built-in inverse cache.

makeCacheMatrix <- function(x = matrix()) {
    # The inverse cache is clear by default
    inverse <- NULL
    
    # Method to set the matrix (and clear the inverse cache)
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # Method to get the matrix
    get <- function() x
    
    # Method to set the inverse cache
    setInverse <- function(i) inverse <<- i
    
    # Method to get the inverse cache
    getInverse <- function() inverse
    
    # The list representing the "object"
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Get the inverse of a CacheMatrix, taking advantage of caching

cacheSolve <- function(x, ...) {
    # Check if the inverse has already been set
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        # If so, use it
        message("getting cached data")
        return(inverse)
    }
    
    # Otherwise, compute, store, and return it
    # NOTE: Passing the ... args through to solve() breaks caching, so I don't
    inverse <- solve(x$get())
    x$setInverse(inverse)
    inverse
}
