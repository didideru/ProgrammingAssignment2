## This set of functions cache the inverse of a given matrix

## This function creates a "matrix" object that sets and gets
## a given matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y){
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinv <- function(inv) im <<- inv
        getinv <- function() im
        list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of a matrix and returns it.
## if the inverse has already been calculated it returns 
## the calculated inverse. 

cacheSolve <- function(x, ...) {
        im <- x$getinv()
        if(!is.null(im)){
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinv(im)
        im      ## Returns a matrix that is the inverse of 'x'
}
