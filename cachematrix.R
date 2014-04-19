## These functions, as a group keep, a cached answer to the matrix inversion of a matrix 
## to minimize the computation burden of repeatedly computing the inverse of the same matrix.

## makeCacheMatrix defines an object of a matrix with its inverse, together with the functions to 
## interact with it

makeCacheMatrix <- function(x = matrix()) {

    x.inv <- NULL
    set <- function(y){
        x <<- y
        x.inv <<- NULL
    }
    get <- function() x
    set.inv <- function(inv = matrix()) x.inv <<- inv
    get.inv <- function() x.inv
    
    list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
}


## cacheSolve works with a matrix object defined by makeCacheMatrix to calculate and cache the object's
## inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get.inv()
    if (!is.null(inv)){
    #    message("Cached inverse passed")  #remove the pound sign if the message is to be printed
        return(inv)
    }
    inv <- solve(x$get(), ...)
    x$set.inv(inv)
    inv
}
