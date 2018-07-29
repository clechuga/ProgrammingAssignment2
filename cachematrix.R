## These functions create a special object that stores a matrix 
## and cache's its inverse.

## The function makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to (1) set
## the value of the matrix, (2) get the value of the matrix,
## (3) set the value of the inverse, and (4) get the value of
## the inverse. 

makeCacheMatrix <- function (x = matrix()) {
        inv <- NULL
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The function cacheSolve calculates the inverse of the special "matrix"
## created by makeCacheMatrix by first checking if the inverse has already
## been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of 'x' and sets 
## the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return (inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}