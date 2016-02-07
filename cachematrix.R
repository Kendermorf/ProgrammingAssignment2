## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## first function creates a list with 4 functions on defined matrix "X"

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## Second function check the cache for the previously calculated inverse matrix
## If we have already calculated it - return the inverse matrix with the message that the data has been gotten from cache
## Otherwise, calculate and print inverse matrix

cacheSolve <- function(x, ...) {
 inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

