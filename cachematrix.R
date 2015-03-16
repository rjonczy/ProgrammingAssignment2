#
# function makeCacheMatrix - creates a special "matrix" object that can cache its inverse
#
#
#
#

makeCacheMatrix <- function(x = matrix()) {
    
    # inverse matrix, initially NULL value
    inverse <- NULL
    
    # function to set x matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # function to get (return) x matrix
    get <- function() x
    
    # function to set inverse of matrix
    setInverse <- function(i) inverse <<- i
    
    # function to get inverse of matrix
    getInverse <- function() inverse
    
    # list of functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#
# function cacheSolve - computes the inverse of special "matrix" returned by makeCacheMatrix function
#                       - if inverse has been already calculated (and matrix has not chnaged) than function returns inverse from cache
#                       - if inverse has not been calculated before (or matrix changed), calculate it and return
#
#

cacheSolve <- function(x, ...) {

    # first we try to get inverse of x and assign to i variable
    i <- x$getInverse()
    
    # if i is not null, it means that we already calculated inverse of matrix,
    # than return it instead of calculating again
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # in this case i is null, so it means that i was not calculated before,
    # than get matrix and assign to variable m
    m <- x$get()
    
    # calculate inverse by calling function solve()
    i <- solve(m, ...)
    
    # set calculated inverse
    x$setInverse(i)
    
    # return calculated inverse
    i
}
