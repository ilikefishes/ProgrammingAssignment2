## These functions will solve the inverse of a matrix
## If a matrix has already been solved it will fetch the cached solution

## Convert a matrix into an object which supports caching

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

## Return a matrix that is the inverse of 'x' (return cached data if exists)

cacheSolve <- function(x, ...) {
              
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
        
}
