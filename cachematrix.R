## Functions for the creating the cache Matrix, storing the calculations 
##of the inverse matrix, and calculating the inverse matrix

## This function will create a cached matrix

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set <- function(y){
                x <<- y
                inv <- NULL
        }
        get <- function () x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function will calculate the inverse matrix or show the stored cached data in case
##this was already calculated

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
