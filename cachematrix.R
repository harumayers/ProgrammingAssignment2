## These functions will create a cached matrix which will store the calculation
## and will calculate the inverse matrix

## This function will create a cached matrix which will store thecomputed
## inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <-NULL
        set <- function(y) {
                x<<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function will calculate the inverse matrix or show the cached data
## if it was already calculated

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
