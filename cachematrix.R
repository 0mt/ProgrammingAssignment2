## This file includes two functions, makeCacheMatrix() and cacheSolve(), which
## compute and cache the inverse of a matrix. Caching the inverse may be
## beneficial in the case of repetitive computations since the inversion is
## typically a time-consuming operation.


## The first function, makeCacheMatrix(), takes a square matrix 'x' and returns
## a list of four functions (setter and getter for 'x'; and setter and getter
## for the inverse of 'x'). Its environment also contains the inverse of 'x'
## which can be later retrieved by the second function, cacheSolve().

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL # initialize an object for storing the inverse
        set <- function(y){
                x <<- y   # assign to 'x' in the parent environment (that of makeCacheMatrix())
                inverse <<- NULL  # clear the cache whenever 'x' is changed
        }
        get <- function() x  # getter for 'x'
        setInverse <- function(xinv) {
                inverse <<- xinv  # setter for the inverse matrix
        }
        getInverse <- function() inverse  # getter for the inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## cacheSolve() takes an object created by makeCacheMatrix() and returns the
## inverse of 'x'.

cacheSolve <- function(x, ...) {
        xinv <- x$getInverse() # read the cached inverse of 'x'
        if (!is.null(xinv)) {  # if the cached value is not a NULL, ...
                message("getting cached data")
                return(xinv) # ... return the inverse and exit the function
        }
        ## if the cached value is NULL, compute and cache the inverse in the 
        ## following steps
        m <- x$get()  # read 'x'
        xinv <- solve(m, ...)  # compute its inverse
        x$setInverse(xinv)  # cache the computed value
        xinv  # return a matrix that is the inverse of 'x'
}
