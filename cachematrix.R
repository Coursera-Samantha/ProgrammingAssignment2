## Creating two functions, one to create a Matrix object, which can later
## be used for calculations, the results of which are stored in cache, and 
## another to solve the calculation in question -- in this case, to calculate
## the inverse matrix of a given matrix object and return the value.

## Creates a Matrix object that can store its inverse in cache.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}


## Computes the inverse matrix of the special Matrix object passed to it
## created by makeCacheMatrix, unless said value is already cached, in which
## case it will simply return the value from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("Getting cached data...")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}


