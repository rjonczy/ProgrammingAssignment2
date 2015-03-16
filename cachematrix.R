
makeCacheMatrix <- function(x = matrix()) {
    
    # inverse matrix, initially NULL value
    inverse <- NULL
    
    # sets x matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # returns x matrix
    get <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}




cacheSolve <- function(x, ...) {

    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setInverse(i)
    i
}
