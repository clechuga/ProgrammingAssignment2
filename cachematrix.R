## These functions create a special object that stores a matrix 
## and cache's its inverse.

## The function makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to (1) set
## the value of the matrix, (2) get the value of the matrix,
## (3) set the value of the inverse, and (4) get the value of
## the inverse. 

makeCacheMatrix <- function (x = matrix()) {
        inv <- NULL
        ## (1) setting the value of the matrix
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        ## (2) getting the value of the matrix
        get <- function() x
        ## (3-4) setting and getting the cached value of the inverse
        ## This is done by using the R function solve() to invert the matrix
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The function cacheSolve computes the inverse of the special "matrix"
## by first checking if the inverse has already been computed. If so, 
## it gets the inverse from the cache and skips the computation. Otherwise, 
## it computes the inverse of 'x' and sets the value of the inverse in the 
## cache via the setinv function defined above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        ## Checking to see if there is a cached inverse value
        if(!is.null(inv)){
                message("getting cached data")
                return (inv)
        }
        ## If no cached inverse is found, the inverse is computed
        data <- x$get()
        inv <- solve(data, ...)
        ## Sets the value of the inverse in the cache via the setinv function
        x$setinv(inv)
        inv
}